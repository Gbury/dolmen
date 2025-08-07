
(* This file is free software, part of dolmen. See file "LICENSE" for more information *)

(* Printing of identifiers *)
(* ************************************************************************* *)

exception Cannot_print of string
(** Exception raised when some input cannot be printed due to lexical
    conventions. In that case, the string contains a message explaining
    why the printing failed. *)

val symbol : Format.formatter -> Dolmen_std.Name.t -> unit
(** Print an identifier, quoting it if necessary. *)

val keyword : Format.formatter -> Dolmen_std.Name.t -> unit
(** Print a keyword. *)

val sanitize : _ Dolmen_intf.Scope.id -> Dolmen_std.Name.t -> Dolmen_std.Name.t
(** Sanitization function to ensure that a name can be printed. *)


(* Printing of Terms and statements *)
(* ************************************************************************* *)

module type S = Dolmen_intf.Print.Smtlib2

module Make
    (Env : Dolmen_intf.Env.Print
     with type name := Dolmen_std.Name.t)
    (S : Dolmen_intf.View.Sexpr.S
     with type id := Dolmen_std.Id.t)
    (V : Dolmen_intf.View.TFF.S
     with type ty = Env.ty
      and type ty_var = Env.ty_var
      and type ty_cst = Env.ty_cst
      and type term = Env.term
      and type term_var = Env.term_var
      and type term_cst = Env.term_cst)
  : S with type env := Env.t
       and type sexpr := S.t
       and type ty := Env.ty
       and type ty_var := Env.ty_var
       and type ty_cst := Env.ty_cst
       and type term := Env.term
       and type term_var := Env.term_var
       and type term_cst := Env.term_cst
