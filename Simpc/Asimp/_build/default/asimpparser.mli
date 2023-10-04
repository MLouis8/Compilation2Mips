
(* The type of tokens. *)

type token = 
  | WHILE
  | VAR
  | TYP_VOID
  | TYP_INT
  | TYP_BOOL
  | STRUCT
  | STAR
  | SET
  | SEMI
  | RPAR
  | RETURN
  | RBRACKET
  | PUTCHAR
  | PLUS
  | NEW
  | LT
  | LPAR
  | LBRACKET
  | IF
  | IDENT of (string)
  | FUNCTION
  | EOF
  | END
  | ELSE
  | DOT
  | CST of (int)
  | COMMA
  | BOOL of (bool)
  | BEGIN

(* This exception is raised by the monolithic API functions. *)

exception Error

(* The monolithic API. *)

val program: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (unit Asimp.program)
