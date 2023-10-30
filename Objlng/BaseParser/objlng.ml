(**
   Annotated abstract syntax for the OBJEling language.
 *)

(* Types of SIMP values *)
type typ =
  | TInt 
  | TBool
  | TClass of string (* class type, identified by its name *)
  | TArray of typ    (* array containing elements of the specified type *)
  | TVoid (* not an actual type in the source language, but having it in
             the AST makes the code more uniform *)

type binop = Add | Mul | Lt

type 'a expression = {
  annot: 'a;
  expr: 'a expr;
}
and 'a expr =
  | Cst   of int
  | Bool  of bool
  | Var   of string
  | Binop of binop * 'a expr * 'a expr
  | Call  of string * 'a expr list
  | MCall of 'a expr * string * 'a expr list
  | New   of string * 'a expr list (* create an instance and call the constructor *)
  | NewTab of typ * 'a expr (* create an array of the given type and size *)
  | Read  of 'a mem               (* read in memory *)
  | This (* current object *)
and 'a mem =
  | Arr of 'a expr * 'a expr (* array access     e1[e2]  *)
  | Atr of 'a expr * string     (* attribute access  o.x    *)

let mk_expr a e = { annot=a; expr=e }

type 'a instruction =
  | Putchar of 'a expr
  | Set     of string * 'a expr
  | If      of 'a expr * 'a sequence * 'a sequence
  | While   of 'a expr * 'a sequence
  | Return  of 'a expr
  | Expr    of 'a expr
  | Write   of 'a mem * 'a expr (*   m = e;   *)
and 'a sequence = 'a instruction list

(* Function definition *)
type 'a function_def = {
  name:   string;
  params: (string * typ) list;
  locals: (string * typ) list;
  code:   'a sequence;
  return: typ;
}

(* Class definition *)
type 'a class_def = {
  name:   string;
  fields: (string * typ) list;
  methods: 'a function_def list;
  parent: string option;
}

(* Program as in SIMP with "structs" upgraded to "classes"  *)
type 'a program = {
  globals:   (string * typ) list;
  functions: 'a function_def list;
  classes:   'a class_def list;
}
