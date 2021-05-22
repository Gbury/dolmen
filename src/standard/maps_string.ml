
(* This file is free software, part of dolmen. See file "LICENSE" for more information *)

(* PARTS: Persistent Adaptive Radix Tree for Strings

   This implementation is heavily inspired by the following paper:

   The Adaptive Radix Tree: ARTful Indexing for Main-Memory Databases
   by Viktor Leis, Alfons Kemper, Thomas Neumann
   https://db.in.tum.de/~leis/papers/ART.pdf

*)

(* Some constants *)
(* ************************************************************************* *)

let dichotomy_threshold = 16
let indirect_threshold = 48

let () =
  assert (indirect_threshold - dichotomy_threshold > 2);
  assert (indirect_threshold < 255);
  ()


(* Type definitions *)
(* ************************************************************************* *)

type 'a t =
  | Leaf of {
      value : 'a option;
    }
  | Prefix of {
      value : 'a option;
      prefix : string;
      child : 'a t;
    }
  | Dichotomy of {
      value : 'a option;
      keys : string;
      children : 'a t array;
    }
  | Indirect of {
      value : 'a option;
      keys : string;
      children : 'a t array;
    }
  | Direct of {
      value : 'a option;
      children : 'a t array;
    }

(* Find *)
(* ************************************************************************* *)

let rec find_aux key len offset t =
  if offset = len then
    match t with
    | Leaf { value; }
    | Prefix { value; _ }
    | Dichotomy { value; _ }
    | Indirect { value; _ }
    | Direct { value; _ } -> value
  else begin
    assert (offset < len);
    match t with
    | Leaf _ -> None
    | Prefix { prefix; child; value = _; } ->
      let n = String.length prefix in
      if offset + n > len then None
      else begin
        let i = ref 0 in
        while !i < n && offset + !i < len &&
              String.unsafe_get prefix !i =
              String.unsafe_get key (offset + !i) do
          i := !i + 1
        done;
        if !i = n
        then find_aux key len (offset + !i) child
        else None
      end
    | Dichotomy { keys; children; value = _; } ->
      let c = String.unsafe_get key offset in
      let n = Array.length children in
      let l = ref 0 in
      let r = ref n in
      while !l < !r do
        let m = (!l + !r) / 2 in
        if String.unsafe_get keys m < c
        then l := m + 1
        else r := m
      done;
      let m = !l in
      if m < n && String.unsafe_get keys m = c
      then find_aux key len (offset + 1) (Array.unsafe_get children m)
      else None
    | Indirect { keys; children; value = _; } ->
      let i = Char.code (String.unsafe_get key offset) in
      let j = Char.code (String.unsafe_get keys i) in
      if j > indirect_threshold then None
      else find_aux key len (offset + 1) (Array.unsafe_get children j)
    | Direct { children; value = _; } ->
      let i = Char.code (String.unsafe_get key offset) in
      find_aux key len (offset + 1) (Array.unsafe_get children i)
  end


(* Creation/Insertion *)
(* ************************************************************************* *)

let[@inline] array_insert a m x =
  let n = Array.length a in
  let a' = Array.make (n + 1) x in
  Array.blit a 0 a' 0 m;
  Array.blit a m a' (m + 1) (n - m);
  a'

let[@inline] string_insert s m c =
  let n = String.length s in
  let b = Bytes.make (n + 1) c in
  Bytes.blit_string s 0 b 0 m;
  Bytes.blit_string s m b (m + 1) (n - m);
  Bytes.unsafe_to_string b

let empty = Leaf { value = None; }

let[@inline] with_value value = function
  | Leaf _ -> Leaf { value; }
  | Prefix { value = _; prefix; child; } -> Prefix { value; prefix; child; }
  | Dichotomy { value = _; keys; children; } -> Dichotomy { value; keys; children; }
  | Indirect { value = _; keys; children } -> Indirect { value; keys; children; }
  | Direct { value = _; children; } -> Direct { value; children; }

let[@inline] mk_prefix value prefix child =
  match prefix, value, child with
  | "", None, _ -> child
  | "", (Some _), _ -> with_value value child
  | _, _, Prefix { prefix = p'; child; value = None; } ->
    let prefix = prefix ^ p' in
    Prefix { prefix; child; value; }
  | _ -> Prefix { prefix; child; value; }

let sub_after s off =
  if off = String.length s then ""
  else String.sub s off (String.length s - off)

let rec add_aux key v len offset t =
  if offset = len then
    with_value (Some v) t
  else begin
    assert (offset < len);
    match t with
    | Leaf { value; } ->
      let prefix = String.sub key offset (len - offset) in
      let child = Leaf { value = Some v; } in
      mk_prefix value prefix child

    | Prefix { prefix; child; value; } ->
      let i = ref 0 in
      let n = String.length prefix in
      while !i < n && offset + !i < len &&
            prefix.[!i] = key.[offset + !i] do
        i := !i + 1
      done;
      if !i = n then begin
        let child = add_aux key v len (offset + !i) child in
        mk_prefix value prefix child
      end else if offset + !i = len then begin
        let pre = String.sub prefix 0 !i in
        let post = String.sub prefix !i (n - !i) in
        mk_prefix value pre (mk_prefix (Some v) post child)
      end else begin
        let pre = String.sub prefix 0 !i in
        let post = sub_after prefix (!i + 1) in
        let post_key = sub_after key (offset + !i + 1) in
        let c0 = String.unsafe_get prefix !i in
        let child0 = mk_prefix None post child in
        let c1 = String.unsafe_get key (offset + !i) in
        let child1 = mk_prefix None post_key (Leaf { value = Some v; }) in
        let c0, child0, c1, child1 =
          if c0 <= c1 then c0, child0, c1, child1 else c1, child1, c0, child0
        in
        let keys = Bytes.make 2 c1 in
        Bytes.unsafe_set keys 0 c0;
        let keys = Bytes.unsafe_to_string keys in
        let children = [| child0; child1; |] in
        let node = Dichotomy { value = None; keys; children; } in
        mk_prefix value pre node
      end

    | Dichotomy { keys; children; value; } ->
      let c = key.[offset] in
      let n = Array.length children in
      let l = ref 0 in
      let r = ref n in
      while !l < !r do
        let m = (!l + !r) / 2 in
        if keys.[m] < c
        then l := m + 1
        else r := m
      done;
      let m = !l in
      if m < n && keys.[m] = c then begin
        let child = add_aux key v len (offset + 1) children.(m) in
        let children = Array.copy children in
        children.(m) <- child;
        Dichotomy { keys; children; value; }
      end else begin
        let key_post = sub_after key (offset + 1) in
        let child = mk_prefix None key_post (Leaf { value = Some v; }) in
        if n < dichotomy_threshold then begin
          let children = array_insert children m child in
          let keys = string_insert keys m c in
          Dichotomy { keys; children; value; }
        end else begin
          let children = Array.append children [| child |] in
          let b = Bytes.make 256 (Char.unsafe_chr 255) in
          Bytes.unsafe_set b (Char.code c) (Char.unsafe_chr n);
          for i = 0 to n - 1 do
            let c = String.unsafe_get keys i in
            Bytes.unsafe_set b (Char.code c) (Char.unsafe_chr i)
          done;
          let keys = Bytes.unsafe_to_string b in
          Indirect { keys; children; value; }
        end
      end

    | Indirect { keys; children; value; } ->
      let i = Char.code (String.unsafe_get key offset) in
      let j = Char.code (String.unsafe_get keys i) in
      if j < 255 then begin
        let child = add_aux key v len (offset + 1) (Array.unsafe_get children j) in
        let children = Array.copy children in
        children.(j) <- child;
        let b = Bytes.of_string keys in
        Bytes.unsafe_set b i (Char.unsafe_chr j);
        let keys = Bytes.unsafe_to_string b in
        Indirect { keys; children; value; }
      end else begin
        let key_post = sub_after key (offset + 1) in
        let child = mk_prefix None key_post (Leaf { value = Some v; }) in
        let n = Array.length children in
        if n < indirect_threshold then begin
          let children = Array.append children [| child |] in
          let b = Bytes.of_string keys in
          Bytes.unsafe_set b i (Char.unsafe_chr n);
          let keys = Bytes.unsafe_to_string b in
          Indirect { keys; children; value; }
        end else begin
          let new_children = Array.make 256 empty in
          Array.unsafe_set new_children i child;
          for c = 0 to 255 do
            let k = Char.code (String.unsafe_get keys c) in
            if k < 255 then
              Array.unsafe_set new_children c (Array.unsafe_get children k)
          done;
          Direct { value; children = new_children; }
        end
      end

    | Direct { children; value; } ->
      let i = Char.code (String.unsafe_get key offset) in
      let child = add_aux key v len (offset + 1) (Array.unsafe_get children i) in
      let children = Array.copy children in
      Array.unsafe_set children i child;
      Direct { children; value; }

  end


(* Updating *)
(* ************************************************************************* *)

let rec find_add_aux f key len offset t =
  if offset = len then
    match t with
    | Leaf { value; _ }
    | Prefix { value; _ }
    | Dichotomy { value; _ }
    | Indirect { value; _ }
    | Direct { value; _ } ->
      with_value (Some (f value)) t
  else begin
    assert (offset < len);
    match t with
    | Leaf { value; } ->
      let v = f None in
      let prefix = String.sub key offset (len - offset) in
      let child = Leaf { value = Some v; } in
      mk_prefix value prefix child

    | Prefix { prefix; child; value; } ->
      let i = ref 0 in
      let n = String.length prefix in
      while !i < n && offset + !i < len &&
            prefix.[!i] = key.[offset + !i] do
        i := !i + 1
      done;
      if !i = n then begin
        let child = find_add_aux f key len (offset + !i) child in
        mk_prefix value prefix child
      end else if offset + !i = len then begin
        let v = f None in
        let pre = String.sub prefix 0 !i in
        let post = String.sub prefix !i (n - !i) in
        mk_prefix value pre (mk_prefix (Some v) post child)
      end else begin
        let v = f None in
        let pre = String.sub prefix 0 !i in
        let post = sub_after prefix (!i + 1) in
        let post_key = sub_after key (offset + !i + 1) in
        let c0 = String.unsafe_get prefix !i in
        let child0 = mk_prefix None post child in
        let c1 = String.unsafe_get key (offset + !i) in
        let child1 = mk_prefix None post_key (Leaf { value = Some v; }) in
        let c0, child0, c1, child1 =
          if c0 <= c1 then c0, child0, c1, child1 else c1, child1, c0, child0
        in
        let keys = Bytes.make 2 c1 in
        Bytes.unsafe_set keys 0 c0;
        let keys = Bytes.unsafe_to_string keys in
        let children = [| child0; child1; |] in
        let node = Dichotomy { value = None; keys; children; } in
        mk_prefix value pre node
      end

    | Dichotomy { keys; children; value; } ->
      let c = key.[offset] in
      let n = Array.length children in
      let l = ref 0 in
      let r = ref n in
      while !l < !r do
        let m = (!l + !r) / 2 in
        if keys.[m] < c
        then l := m + 1
        else r := m
      done;
      let m = !l in
      if m < n && keys.[m] = c then begin
        let child = find_add_aux f key len (offset + 1) children.(m) in
        let children = Array.copy children in
        children.(m) <- child;
        Dichotomy { keys; children; value; }
      end else begin
        let v = f None in
        let key_post = sub_after key (offset + 1) in
        let child = mk_prefix None key_post (Leaf { value = Some v; }) in
        if n < dichotomy_threshold then begin
          let children = array_insert children m child in
          let keys = string_insert keys m c in
          Dichotomy { keys; children; value; }
        end else begin
          let children = Array.append children [| child |] in
          let b = Bytes.make 256 (Char.unsafe_chr 255) in
          Bytes.unsafe_set b (Char.code c) (Char.unsafe_chr n);
          for i = 0 to n - 1 do
            let c = String.unsafe_get keys i in
            Bytes.unsafe_set b (Char.code c) (Char.unsafe_chr i)
          done;
          let keys = Bytes.unsafe_to_string b in
          Indirect { keys; children; value; }
        end
      end

    | Indirect { keys; children; value; } ->
      let i = Char.code (String.unsafe_get key offset) in
      let j = Char.code (String.unsafe_get keys i) in
      if j < 255 then begin
        let child = find_add_aux f key len (offset + 1) (Array.unsafe_get children j) in
        let children = Array.copy children in
        children.(j) <- child;
        let b = Bytes.of_string keys in
        Bytes.unsafe_set b i (Char.unsafe_chr j);
        let keys = Bytes.unsafe_to_string b in
        Indirect { keys; children; value; }
      end else begin
        let v = f None in
        let key_post = sub_after key (offset + 1) in
        let child = mk_prefix None key_post (Leaf { value = Some v; }) in
        let n = Array.length children in
        if n < indirect_threshold then begin
          let children = Array.append children [| child |] in
          let b = Bytes.of_string keys in
          Bytes.unsafe_set b i (Char.unsafe_chr n);
          let keys = Bytes.unsafe_to_string b in
          Indirect { keys; children; value; }
        end else begin
          let new_children = Array.make 256 empty in
          Array.unsafe_set new_children i child;
          for c = 0 to 255 do
            let k = Char.code (String.unsafe_get keys c) in
            if k < 255 then
              Array.unsafe_set new_children c (Array.unsafe_get children k)
          done;
          Direct { value; children = new_children; }
        end
      end

    | Direct { children; value; } ->
      let i = Char.code (String.unsafe_get key offset) in
      let child = find_add_aux f key len (offset + 1) (Array.unsafe_get children i) in
      let children = Array.copy children in
      Array.unsafe_set children i child;
      Direct { children; value; }

  end


(* Iteration *)
(* ************************************************************************* *)

let rec sum_lengths acc = function
  | [] -> acc
  | s :: r -> sum_lengths (String.length s + acc) r

let rec rev_unsafe_blits b pos = function
  | [] -> ()
  | s :: r ->
    let n = String.length s in
    Bytes.blit_string s 0 b (pos - n) n;
    rev_unsafe_blits b (pos - n) r

let rev_concat = function
  | [] -> ""
  | l ->
    let n = sum_lengths 0 l in
    let b = Bytes.create n in
    rev_unsafe_blits b n l;
    Bytes.unsafe_to_string b

let iter_apply f acc = function
  | None -> ()
  | Some v ->
    let s = rev_concat acc in
    f s v

let rec iter_aux f acc t =
  match t with
  | Leaf { value; } ->
    iter_apply f acc value
  | Prefix { value; prefix; child; } ->
    iter_apply f acc value;
    iter_aux f (prefix :: acc) child
  | Dichotomy { value; keys; children; } ->
    iter_apply f acc value;
    String.iteri (fun i c ->
        let s = String.make 1 c in
        let child = children.(i) in
        iter_aux f (s :: acc) child
      ) keys
  | Indirect { value; keys; children; } ->
    iter_apply f acc value;
    String.iter (fun c ->
        let i = Char.code c in
        if i < 255 then begin
          let s = String.make 1 c in
          let child = children.(i) in
          iter_aux f (s :: acc) child
        end) keys
  | Direct { value; children; } ->
    iter_apply f acc value;
    Array.iteri (fun i child ->
        let c = Char.chr i in
        let s = String.make 1 c in
        iter_aux f (s :: acc) child
      ) children


(* Exported interface *)
(* ************************************************************************* *)

let empty = empty

let find_opt k t =
  find_aux k (String.length k) 0 t

let find_exn k t =
  match find_opt k t with
  | None -> raise Not_found
  | Some res -> res

let add k v t =
  add_aux k v (String.length k) 0 t

let find_add k f t =
  find_add_aux f k (String.length k) 0 t

let iter f t = iter_aux f [] t

let fold f t acc =
  let r = ref acc in
  iter_aux (fun s v -> r := f s v !r) [] t;
  !r



