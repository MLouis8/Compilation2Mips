open Msimp

(* types for various environments *)
module Env = Map.Make(String)
type tenv = Msimp.typ Env.t
type fenv = Msimp.function_def Env.t
type senv = Msimp.struct_def Env.t

(* utility function *)
let check actual expected =
  if actual <> expected then failwith "type error"

let add2env l env =
  List.fold_left (fun env (x, t) -> Env.add x t env) env l

(* main typing function *)
let type_program (p: program): unit =

  (* initialize global environments *)
  let tenv = add2env p.globals Env.empty in
  let fenv = add2env (List.map (fun (f: function_def) -> f.name, f) p.functions) Env.empty in
  let senv = add2env (List.map (fun s -> s.name, s) p.structs) Env.empty in

  (* checking types for a function definition *)
  let type_fdef fdef =

    (* add local elements to the environments *)
    let tenv = failwith "not implemented" in

    (* note: nested definitions ensure that all environments are known to the
       inner functions, without making them explicit arguments *)

    (* typing an expression *)
    let rec type_expr: expression -> typ = function
      | Cst n -> TInt
      | Bool b -> TBool
      | Var x -> Env.find x tenv
    and type_mem m = failwith "not implemented"
    in

    (* typing instructions *)
    let rec type_seq s = List.iter type_instr s
    and type_instr = function
      | Putchar e     -> check (type_expr e) TInt
    in
    type_seq fdef.code
  in
  List.iter type_fdef p.functions
  

