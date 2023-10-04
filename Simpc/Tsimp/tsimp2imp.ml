let tr_op: Simp.binop -> Imp.binop = function
  | Add -> Add
  | Mul -> Mul
  | Lt  -> Lt

(* main translation function *)
let translate_program (p: Tsimp.program): Imp.program =

  (* translate an expression *)
  let rec tr_expr (te: Tsimp.expression): Imp.expression = match te.expr with
    | Cst n  -> Cst n
    | Bool b -> Bool b
    | Var x  -> Var x
    | Binop(op, e1, e2) -> Binop(tr_op op, tr_expr e1, tr_expr e2)
  and tr_mem m = failwith "not implemented"
  in

  (* translate instructions *)
  let rec tr_seq s = List.map tr_instr s
  and tr_instr: Tsimp.instruction -> Imp.instruction = function
    | Putchar e     -> Putchar(tr_expr e)
  in

  (* translate function definitions *)
  let tr_fdef (fdef: Tsimp.function_def): Imp.function_def =
    { Imp.name = fdef.name; 
      params = List.map fst fdef.params; 
      locals = List.map fst fdef.locals; 
      code = tr_seq fdef.code;
    }
  in

  { Imp.globals = List.map fst p.globals;
    functions = List.map tr_fdef p.functions }
