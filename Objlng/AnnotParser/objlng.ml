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

let typ_to_string (t: typ): string =
  match t with
  | TInt -> "TInt"
  | TBool -> "TBool"
  | TClass x -> "TClass "^x
  | TArray _ -> "TArray"
  | TVoid -> "TVoid"

type binop = Add | Mul | Lt

type 'a expression = {
  annot: 'a;
  expr: 'a expr;
}
and 'a expr =
  | Cst   of int
  | Bool  of bool
  | Var   of string
  | Binop of binop * 'a expression * 'a expression
  | Call  of string * 'a expression list
  | MCall of 'a expression * string * 'a expression list
  | New   of string * 'a expression list (* create an instance and call the constructor *)
  | NewTab of typ * 'a expression (* create an array of the given type and size *)
  | Read  of 'a mem               (* read in memory *)
  | This (* current object *)
  | Super (* parent object *)
  | Instanceof of 'a expression * string (* is obj from class c *)
  | Cast of 'a expression * string (* cast obj to class c *)
and 'a mem =
  | Arr of 'a expression * 'a expression (* array access     e1[e2]  *)
  | Atr of 'a expression * string        (* attribute access  o.x    *)

let expr_to_string (expr: typ expression) = match  expr.expr with
  | Cst _ -> "Cst"
  | Bool _ -> "Bool"
  | Var _ -> "Var"
  | Binop _ -> "Binop"
  | Call _ -> "Call"
  | MCall _ -> "MCall"
  | New _ -> "New"
  | NewTab _ -> "Newtab"
  | Read _ -> "Read"
  | This -> "This"
  | Super -> "Super"
  | Instanceof _ -> "Instanceof"
  | Cast _ -> "Cast"
let mk_expr a e = { annot=a; expr=e }

type 'a instruction =
  | Putchar of 'a expression
  | Set     of string * 'a expression
  | If      of 'a expression * 'a sequence * 'a sequence
  | While   of 'a expression * 'a sequence
  | Return  of 'a expression
  | Expr    of 'a expression
  | Write   of 'a mem * 'a expression (*   m = e;   *)
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

let find_class (t: typ) (classes: 'a class_def list): 'a class_def =
  match t with
  | TClass cname -> List.find (fun (cdef: 'a class_def) -> cdef.name = cname) classes
  | _ -> failwith ("class not found: "^(typ_to_string t))

let has_get_parent (cdef: 'a class_def) (classes: 'a class_def list): bool * 'a class_def =
  match cdef.parent with
  | Some parent -> (true, find_class (TClass parent) classes)
  | None -> (false, cdef)

let rec is_instance_of (obj: typ) (c: string) (classes: 'a class_def list) =
  let cname = match obj with TClass c -> c | _ -> assert false in
  if cname = c
    then true
  else
    let c_def = find_class (TClass cname) classes in 
    let inheritance = has_get_parent c_def classes in
    if fst inheritance
      then is_instance_of (TClass (snd inheritance).name) c classes
    else false
    
(* Program as in IMP + types + user-defined  *)
type 'a program = {
  globals:   (string * typ) list;
  functions: 'a function_def list;
  classes:   'a class_def list;
}
