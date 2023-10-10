let tr_op: Asimp.binop -> Imp.binop = function
  | Add -> Add
  | Mul -> Mul
  | Lt  -> Lt

(* main translation function *)
let translate_program (p: Asimp.typ Asimp.program) =

  (* translation of an expression *)
  let rec tr_expr (te: Asimp.typ Asimp.expression): Imp.expression = match te.expr with
    | Cst n             -> Cst n
    | Bool b            -> Bool b
    | Var x             -> Var x
    | Binop(op, e1, e2) -> Binop(tr_op op, tr_expr e1, tr_expr e2)
    | Call(x, l)        -> Call(x, List.map tr_expr l)
    | New x             -> Alloc()
    | NewTab(t, e)      -> Alloc()
    | Read m            -> Deref(tr_mem m)
  and tr_mem m = failwith "not implemented"
  in

  (* translation of instructions *)
  let rec tr_seq s = List.map tr_instr s
  and tr_instr: Asimp.typ Asimp.instruction -> Imp.instruction = function
    | Putchar e     -> Putchar(tr_expr e)
    | Set(x, e)     -> Set(x, tr_expr e)
    | If(b, s1, s2) -> If(tr_expr b, List.map tr_instr s1, List.map tr_instr s2)
    | While(e, s)   -> While(tr_expr e, List.map tr_instr s)
    | Return e      -> Return(tr_expr e)
    | Expr e        -> Expr(tr_expr e)
    | Write(m, e)   -> failwith "not implemented"
  in

  (* translation of function definitions *)
  let tr_fdef (fdef: Asimp.typ Asimp.function_def) =
    { Imp.name = fdef.name; 
      params = List.map fst fdef.params; 
      locals = List.map fst fdef.locals; 
      code = tr_seq fdef.code;
    }
  in

  { Imp.globals = List.map fst p.globals;
    functions = List.map tr_fdef p.functions }
