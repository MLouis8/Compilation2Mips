(**
   Minimally and mutably annotated abstract syntax for the SIMP language.
   Imp + user-defined structures and arrays, but no explicit pointer manipulation
 *)

(* Types of SIMP values *)
type typ =
  | TInt 
  | TBool
  | TStruct of string (* struct type, identified by its name *)
  | TArray of typ     (* array containing elements of the specified type *)
  | TVoid (* not an actual type in the source language, but having it in
             the AST makes the code more uniform *)

type binop = Add | Mul | Lt

type expression =
  | Cst   of int
  | Bool  of bool
  | Var   of string
  | Binop of binop * expression * expression
  | Call  of string * expression list
  (* No explicit Deref or Alloc *)
  | New   of string            (* create a struct of the given name; fields are not initialized *)
  | NewTab of typ * expression (* create an array of the given type and size *)
  | Read  of mem               (* read in memory *)
and mem =
  | Arr of expression * expression (* array access   e1[e2]   *)
  | Str of expression * string * int option ref (* field access    s.x     *)
             (* third parameter, when defined, gives the index of the field *)
             (* initialized undefined (ref None), then updated during typechecking *)

type instruction =
  | Putchar of expression
  | Set     of string * expression
  | If      of expression * sequence * sequence
  | While   of expression * sequence
  | Return  of expression
  | Expr    of expression
  | Write   of mem * expression (*   m = e;   *)
and sequence = instruction list

(* Function definition is now annotated by types *)
type function_def = {
  name:   string;
  params: (string * typ) list;
  locals: (string * typ) list;
  code:   sequence;
  return: typ;
}

(* User-defined structure, with a name and typed fields *)
type struct_def = {
  name:   string;
  fields: (string * typ) list;
}

(* Program as in IMP + types + user-defined structs *)
type program = {
  globals:   (string * typ) list;
  functions: function_def list;
  structs:   struct_def list;
}
