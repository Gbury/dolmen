(*
Copyright (c) 2013, Simon Cruanes
Copyright (c) 2016, Guillaume Bury
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

Redistributions of source code must retain the above copyright notice, this
list of conditions and the following disclaimer.  Redistributions in binary
form must reproduce the above copyright notice, this list of conditions and the
following disclaimer in the documentation and/or other materials provided with
the distribution.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*)

(** {1 TPTP Lexer} *)

{
  open Tokens_tptp

  exception Error
}

let printable_char = [^ '\n']
let not_star_slash = ([^ '*']* '*'+ [^ '/' '*'])* [^ '*']*
let comment_line = ['%' '#'] printable_char*
let comment_block = '/' '*' not_star_slash '*' '/'
let comment = comment_line | comment_block

let sq_char = [^ '\\' '''] | "\\\\" | "\\'"
let do_char = [^ '"' '\\' ] |  "\\\\" | "\\\""
let single_quoted = ''' sq_char+ '''
let distinct_object = '"' do_char* '"'

let zero_numeric = '0'
let non_zero_numeric = ['1' - '9']
let numeric = ['0' - '9']
let sign = ['+' '-']

let dot_decimal = '.' numeric +
let positive_decimal = non_zero_numeric numeric*
let decimal = zero_numeric | positive_decimal
let unsigned_integer = decimal
let signed_integer = sign unsigned_integer
let integer = signed_integer | unsigned_integer
let decimal_fraction = decimal dot_decimal
let decimal_exponent = (decimal | decimal_fraction) ['e' 'E'] integer
let unsigned_real = decimal_fraction | decimal_exponent
let signed_real = sign unsigned_real
let real = signed_real | unsigned_real
let unsigned_rational = decimal '/' positive_decimal
let signed_rational = sign unsigned_rational
let rational = signed_rational | unsigned_rational

let lower_alpha = ['a' - 'z']
let upper_alpha = ['A' - 'Z']
let alpha_numeric = lower_alpha | upper_alpha | numeric | '_'

let upper_word = upper_alpha alpha_numeric*
let lower_word = lower_alpha alpha_numeric*
let dollar_word = '$' lower_word
let dollar_dollar_word = "$$" lower_word

rule token = parse
  | comment
    { String.iter (function
        | '\n' -> Lexing.new_line lexbuf
        | _ -> ()
      ) (Lexing.lexeme lexbuf);
      token lexbuf }

  | '\n' { Lexing.new_line lexbuf; token lexbuf }
  | [' ' '\t' '\r'] { token lexbuf }
  | eof { EOF }

  | '.'   { DOT }
  | ','   { COMMA }
  | ':'   { COLON }

  | '('   { LEFT_PAREN }
  | ')'   { RIGHT_PAREN }
  | '['   { LEFT_BRACKET }
  | ']'   { RIGHT_BRACKET }

  | '^'   { LAMBDA }
  | '@'   { APPLY }
  | "@+"  { INDEFINITE_DESCRIPTION }
  | "@-"  { DEFINITE_DESCRIPTION }
  | "!>"  { FORALL_TY }
  | '!'   { FORALL }
  | "?*"  { EXISTS_TY }
  | '?'   { EXISTS }

  | "!!"  { PI }
  | "??"  { SIGMA }

  | '<'   { LESS }
  | '>'   { ARROW }

  | '*'   { STAR }
  | '+'   { PLUS }

  | "<~>" { XOR }
  | "<=>" { EQUIV }
  | "=>"  { IMPLY }
  | "<="  { LEFT_IMPLY }

  | '~'   { NOT }
  | '&'   { AND }
  | '|'   { VLINE }
  | "~&"  { NOTAND }
  | "~|"  { NOTVLINE }

  | '='   { EQUAL }
  | "!="  { NOT_EQUAL }
  | "-->" { GENTZEN_ARROW }


  | lower_word {
    match Lexing.lexeme lexbuf with
    | "cnf" -> CNF
    | "fof" -> FOF
    | "tff" -> TFF
    | "thf" -> THF
    | "tpi" -> TPI
    | "include" -> INCLUDE
    | s -> LOWER_WORD(s)
  }
  | dollar_word {
    match Lexing.lexeme lexbuf with
    | "$cnf" -> DOLLAR_CNF
    | "$fof" -> DOLLAR_FOF
    | "$tff" -> DOLLAR_TFF
    | "$thf" -> DOLLAR_THF
    | "$fot" -> DOLLAR_FOT
    | "$ite_f" -> ITE_F
    | "$ite_t" -> ITE_T
    | "$let_tf" -> LET_TF
    | "$let_ft" -> LET_FT
    | "$let_ff" -> LET_FF
    | "$let_tt" -> LET_TT
    | s -> DOLLAR_WORD(s)
  }
  | upper_word          { UPPER_WORD(Lexing.lexeme lexbuf) }
  | dollar_dollar_word  { DOLLAR_DOLLAR_WORD(Lexing.lexeme lexbuf) }
  | single_quoted       { SINGLE_QUOTED(Lexing.lexeme lexbuf) }
  | distinct_object     { DISTINCT_OBJECT(Lexing.lexeme lexbuf) }
  | integer             { INTEGER(Lexing.lexeme lexbuf) }
  | rational            { RATIONAL(Lexing.lexeme lexbuf) }
  | real                { REAL(Lexing.lexeme lexbuf) }

  | _ { raise Error }

