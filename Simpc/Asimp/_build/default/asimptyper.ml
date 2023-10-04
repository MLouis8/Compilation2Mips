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
      | Cst n -> mk_expr TInt (Cst n)
      | Bool b -> mk_expr TBool (Bool b)
      | Var x -> mk_expr (Env.find x tenv) (Var x)
    and type_mem m = failwith "not implemented"
    in

    (* type instructions *)
    let rec type_seq s = List.map type_instr s
    and type_instr = function
      | Putchar e     -> Putchar (check (type_expr e) TInt)
    in
    { fdef with code = type_seq fdef.code }
  in
  { p with functions = List.map type_fdef p.functions }
