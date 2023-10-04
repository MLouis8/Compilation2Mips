open Asimp

(* types for various environments *)
module Env = Map.Make(String)
type tenv = typ Env.t
type fenv = unit function_def Env.t
type senv = struct_def Env.t

(* utility function *)
let check te t =
  if te.annot <> t then failwith "type error";
  te

let add2env l env =
  List.fold_left (fun env (x, t) -> Env.add x t env) env l

(* main typing function *)
let type_program (p: unit program): typ program =

  (* initialize global environments *)
  let tenv = add2env p.globals Env.empty in
  let fenv = add2env (List.map (fun (f: unit function_def) -> f.name, f) p.functions) Env.empty in
  let senv = add2env (List.map (fun s -> s.name, s) p.structs) Env.empty in

  (* typing a function definition *)
  let type_fdef fdef =
    (* add local elements to the environments *)
    let tenv = failwith "not implemented" in

    (* note: nested definitions ensure that all environments are known to the
       inner functions, without making them explicit arguments *)

    (* type expressions *)
    let rec type_expr (e: unit expression): typ expression = match e.expr with
      | Cst n             -> mk_expr TInt (Cst n)
      | Bool b            -> mk_expr TBool (Bool b)
      | Var x             -> mk_expr (Env.find x tenv) (Var x)
      | Binop(op, e1, e2) -> mk_expr TInt (Binop(op, check (type_expr e1) TInt, check (type_expr e2) TInt))
      | Call(x, l)        -> mk_expr TVoid (Call(x, List.map type_expr l)) (* not sure about this one, TVoid should be function type *)
      | New x             -> mk_expr (TStruct x) (New x)
      | NewTab(t, e)      -> mk_expr (TArray t) (NewTab(t, check (type_expr e) t))
      | Read m            -> failwith "not implemented"
    and type_mem m = TVoid
    in

    (* type instructions *)
    let rec type_seq s = List.map type_instr s
    and type_instr = function
      | Putchar e     -> Putchar (check (type_expr e) TInt)
      | Set(x, e)     -> Set (x, (check (type_expr e) (Env.find x tenv)))
      | If(b, s1, s2) -> If (check (type_expr b) TBool, type_seq s1, type_seq s2)
      | While(e, s)   -> While (check (type_expr e) TBool, type_seq s)
      | Return e      -> Return (type_expr e) (* maybe return types should match to function definition *)
      | Expr e        -> Expr (type_expr e)
      | Write(m, e)   -> failwith "not implemented"
    in
    { fdef with code = type_seq fdef.code }
  in
  { p with functions = List.map type_fdef p.functions }
