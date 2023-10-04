let tr_op: Simp.binop -> Imp.binop = function
  | Add -> Add
  | Mul -> Mul
  | Lt  -> Lt

(* types for various environments *)
module Env = Map.Make(String)
type tenv = Simp.typ Env.t
type fenv = Simp.function_def Env.t
type senv = Simp.struct_def Env.t

let add2env l env =
  List.fold_left (fun env (x, t) -> Env.add x t env) env l

(* main translation function *)
let translate_program (p: Simp.program) =

  (* initialize global environments *)
  let tenv = add2env p.globals Env.empty in
  let fenv = add2env (List.map (fun (f: Simp.function_def) -> f.name, f) p.functions) Env.empty in
  let senv = add2env (List.map (fun s -> s.Simp.name, s) p.structs) Env.empty in

  (* translation function for a function definition *)
  let tr_fdef (fdef: Simp.function_def) =
    (* add local elements to the environments *)
    let tenv = failwith "not implemented" in

    (* note: nested definitions ensure that all environments are known to the
       inner functions, without making them explicit arguments *)

    (* typing an expression *)
    let rec type_expr: Simp.expression -> Simp.typ = function
      | Cst n -> TInt
      | Bool b -> TBool
      | Var x -> Env.find x tenv
    and type_mem m =
      failwith "not implemented"
    in

    (* translation of an expression *)
    let rec tr_expr: Simp.expression -> Imp.expression = function
      | Cst n  -> Cst n
      | Bool b -> Bool b
      | Var x  -> Var x
      | Binop(op, e1, e2) -> Binop(tr_op op, tr_expr e1, tr_expr e2)
    and tr_mem m = failwith "not implemented"
    in

    (* translation of instructions *)
    let rec tr_seq s = List.map tr_instr s
    and tr_instr: Simp.instruction -> Imp.instruction = function
      | Putchar e     -> Putchar(tr_expr e)
    in

    { Imp.name = fdef.name; 
      params = List.map fst fdef.params; 
      locals = List.map fst fdef.locals; 
      code = tr_seq fdef.code;
    }
  in

  { Imp.globals = List.map fst p.globals;
    functions = List.map tr_fdef p.functions }
