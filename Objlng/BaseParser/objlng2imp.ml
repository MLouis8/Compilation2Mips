let tr_op: Objlng.binop -> Imp.binop = function
  | Add -> Add
  | Mul -> Mul
  | Lt  -> Lt

(* types for various environments *)
module Env = Map.Make(String)

let add2env l env =
  List.fold_left (fun env (x, t) -> Env.add x t env) env l

(* main translation function *)
let translate_program (p: Objlng.program) =
  let tenv = add2env p.globals Env.empty in

  let tr_fdef (fdef: Objlng.function_def) = 
    let tenv = failwith "not implemented" in

    (* translation of an expression *)
    let rec tr_expr: Objlng.expression -> Imp.expression = function
      | Cst n  -> Cst n
      | Bool b -> Bool b
      | Var x  -> Var x
      | Binop(op, e1, e2) -> Binop(tr_op op, tr_expr e1, tr_expr e2)
      | This -> failwith "not implemented"
      | NewTab(t, e) -> failwith "not implemented"
      | Read m -> failwith "not implemented"
      | _ -> failwith "Error, tr_instr doesn't take care of special expressions"
    and tr_mem m = failwith "not implemented"
    in

    (* translation of instructions *)
    let rec tr_seq s = List.map tr_instr s
    and tr_instr: Objlng.instruction -> Imp.instruction = function
      | Putchar e     -> Putchar(tr_expr e)
      | If(e, s1, s2) -> failwith "not implemented"
      | While(e, s) -> failwith "not implemented"
      | Return e -> failwith "not implemented"
      | Expr e -> Expr(tr_expr e)
      | Set(x1, New(x2, l)) -> failwith "not implemented"
      | Set(x1, Call(x2, l)) -> failwith "not implemented"
      | Set(x1, MCall(e, x2, l)) -> failwith "not implemented"
      | Set(x, e) -> failwith "not implemented"
      | Write(m, New(x, l)) -> failwith "not implemented"
      | Write(m, Call(x, l)) -> failwith "not implemented"
      | Write(m, MCall(e, x, l)) -> failwith "not implemented"
      | Write(m, e) -> failwith "not implemented"
    in
    { Imp.name = fdef.name; 
    params = List.map fst fdef.params; 
    locals = List.map fst fdef.locals; 
    code = tr_seq fdef.code;
  }
in
  { Imp.globals = List.map fst p.globals;
    functions = List.map tr_fdef p.functions; }