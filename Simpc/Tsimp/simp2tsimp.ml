open Simp

(* types for various environments *)
module Env = Map.Make(String)
type tenv = typ Env.t
type fenv = function_def Env.t
type senv = struct_def Env.t

(* utility function *)
let check (te: Tsimp.expression) t =
  if te.typ <> t then failwith "type error"; 
  te

let add2env l env =
  List.fold_left (fun env (x, t) -> Env.add x t env) env l

(* main typing function *)
let translate_program (p: Simp.program): Tsimp.program =

  (* initialize global environments *)
  let tenv = add2env p.globals Env.empty in
  let fenv = add2env (List.map (fun (f: function_def) -> f.name, f) p.functions) Env.empty in
  let senv = add2env (List.map (fun s -> s.name, s) p.structs) Env.empty in

  (* typing a function definition *)
  let tr_fdef (fdef: Simp.function_def): Tsimp.function_def =
    (* add local elements to the environments *)
    let tenv = failwith "not implemented" in

    (* note: nested definitions ensure that all environments are known to the
       inner functions, without making them explicit arguments *)

    (* typing an expression *)
    let rec tr_expr: Simp.expression -> Tsimp.expression = function
      | Cst n -> Tsimp.mk_expr TInt (Cst n)
      
      | Bool b -> Tsimp.mk_expr TBool (Bool b)
      | Var x -> Tsimp.mk_expr (Env.find x tenv) (Var x)
      | Binop bop, e1, e2 -> Tsimp.mk_expr bop (Binop (bop, e1, e2))
      | Call
    and tr_mem m = failwith "not implemented"
    in

    (* typing instructions *)
    let rec tr_seq s = List.map tr_instr s
    and tr_instr: Simp.instruction -> Tsimp.instruction = function
      | Putchar e     -> Putchar (check (tr_expr e) TInt)
    in

    { name = fdef.name;
      params = fdef.params;
      locals = fdef.locals;
      code = tr_seq fdef.code;
      return = fdef.return;
    }
  in

  { globals = p.globals;
    structs = p.structs;
    functions = List.map tr_fdef p.functions }
  

