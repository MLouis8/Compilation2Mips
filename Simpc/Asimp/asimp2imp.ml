let tr_op: Asimp.binop -> Imp.binop = function
  | Add -> Add
  | Mul -> Mul
  | Lt  -> Lt

let rec typ2byt: Asimp.typ -> int = function
  | TInt      -> 4
  | TBool     -> 1
  | TStruct x -> 0 (* I want to alloc according to the struct composition ! *)
  | TArray t  -> typ2byt t
  | TVoid     -> raise(Invalid_argument "TVoid isn't a valid type")
(* main translation function *)
let translate_program (p: Asimp.typ Asimp.program) =

  (* translation of an expression *)
  let rec tr_expr (te: Asimp.typ Asimp.expression): Imp.expression = match te.expr with
    | Cst n             -> Cst n
    | Bool b            -> Bool b
    | Var x             -> Var x
    | Binop(op, e1, e2) -> Binop(tr_op op, tr_expr e1, tr_expr e2)
    | Call(x, l)        -> Call(x, List.map tr_expr l)
    | New x             -> Alloc(Cst (typ2byt te.annot))
    | NewTab(t, e)      -> Alloc(Binop(Mul, tr_expr e, Cst (typ2byt te.annot)))
    | Read(Arr(e1, e2)) -> Imp.array_access (tr_expr e1) (tr_expr e2)
    | Read(Str(e, x))   -> match te.annot with begin
                            | TStruct str_name -> failwith "not implemented"(* retrieve field x of the structure named str_name in the structures "env" *)
                            | _ -> raise(Invalid_argument "Should be a TStruct here") end
  and tr_mem: (e: Asimp.expression, x: string) -> Imp.expression = failwith "not implemented"
  (* only for Str(e, x) *)
    
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
    | Write(m, e)   -> match m with
                        | Arr(e1, e2) -> Imp.array_write (tr_expr e1) (tr_expr e2) (tr_expr e)
                        | Str(e1, x)   -> Write(tr_mem e1 x, tr_expr e)
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
    functions = List.map tr_fdef p.functions;
    (* Can we put the structures definitions here ? *)}
