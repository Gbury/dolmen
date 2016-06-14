{
  open Tokens_dedukti

  exception Error

  let set_module_name, get_module_name =
    let l = ref [] in
    let set lexbuf s =
      let tmp = List.remove_assq lexbuf !l in
      l := (lexbuf, name) :: tmp
    in
    let get lexbuf =
      try List.assq lexbuf !l
      with Not_found -> ""
    in
    set, get
}

let space   = [' ' '\t']
let modname = ['a'-'z' 'A'-'Z' '0'-'9' '_']+
let ident   = ['a'-'z' 'A'-'Z' '0'-'9' '_']['a'-'z' 'A'-'Z' '0'-'9' '_' '!' '?' '\'' ]*
let capital = ['A'-'Z']+

rule token = parse
  | space       { token lexbuf  }
  | '\n'        { new_line lexbuf ; token lexbuf }
  | "(;"        { comment lexbuf }
  | '.'         { DOT }
  | ','         { COMMA }
  | ':'         { COLON }
  | '['         { LEFTSQU }
  | ']'         { RIGHTSQU }
  | '{'         { LEFTBRA }
  | '}'         { RIGHTBRA }
  | '('         { LEFTPAR }
  | ')'         { RIGHTPAR }
  | "-->"       { LONGARROW }
  | "->"        { ARROW }
  | "=>"        { FATARROW }
  | ":="        { DEF }
  | "_"         { UNDERSCORE }
  | "Type"      { TYPE }
  | "#WHNF"     { WHNF }
  | "#HNF"      { HNF }
  | "#SNF"      { SNF }
  | "#STEP"     { STEP }
  | "#INFER"    { INFER }
  | "#CONV"     { CONV }
  | "#CHECK"    { CHECK }
  | "#PRINT"    { PRINT }
  | "#GDT"      { GDT }

  | "#NAME" space+ (modname as md)
    { set_module_name lexbuf md; NAME md }

  | '#' (capital as cmd)
    { OTHER cmd }

  | modname as md '.' (ident as id)
    { ID (md, id) }
  | ident  as id
    { ID (get_module_name lexbuf, id) }

  | '"'
    { string (Buffer.create 42) lexbuf }

  | eof
    { EOF }
  | _
    { raise Error }

 and comment = parse
  | ";)"  { token lexbuf }
  | '\n'  { new_line lexbuf ; comment lexbuf }
  | _     { comment lexbuf }
  | eof   { raise Error }

and string buf = parse
  | '"'           { STRING (Buffer.contents buf) }
  | '\\' (_ as c) { Buffer.add_char buf '\\';
                    Buffer.add_char c;
                    string buf lexbuf }
  | '\n'          { Lexing.new_line lexbuf ;
                    Buffer.add_char buf '\n';
                    string buf lexbuf }
  | _ as c        { Buffer.add_char c;
                    string buf lexbuf }
  | eof           { raise Error }


