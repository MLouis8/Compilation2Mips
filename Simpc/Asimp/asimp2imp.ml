let tr_op: Asimp.binop -> Imp.binop = function
  | Add -> Add
  | Mul -> Mul
  | Lt  -> Lt

let find_struct (struct_n: string) (sl: Asimp.struct_def list): Asimp.struct_def =
  List.find (fun (sd: Asimp.struct_def) -> sd.name = struct_n) sl

  (* main translation function *)
let translate_program (p: Asimp.typ Asimp.program) =
  let rec typ2byt: Asimp.typ -> int = function
    | TInt      -> 4
    | TBool     -> 1
    | TStruct x ->
      let def = find_struct x p.structs in
      List.fold_left (fun cpt p -> cpt + typ2byt (snd p)) 0 def.fields
    | TArray t  -> typ2byt t
    | TVoid     -> raise(Invalid_argument "TVoid isn't a valid type")
    in
  let struct_offset f field_n =
    let rec field_offset = function
      | [] -> failwith "Error: field not found !"
      | field :: tail when fst field = field_n -> 0
      | field :: tail -> typ2byt (snd field) + field_offset tail
    in field_offset f
  in
  (* translation of an expression *)
  let rec tr_expr (te: Asimp.typ Asimp.expression): Imp.expression = match te.expr with
    | Cst n             -> Cst n
    | Bool b            -> Bool b
    | Var x             -> Var x
    | Binop(op, e1, e2) -> Binop(tr_op op, tr_expr e1, tr_expr e2)
    | Call(x, l)        -> Call(x, List.map tr_expr l)
    | New x             -> Alloc(Cst (typ2byt (TStruct x)))
    | NewTab(t, e)      -> Alloc(Binop(Mul, tr_expr e, Cst (typ2byt te.annot)))
    | Read(Arr(e1, e2)) -> Imp.array_access (tr_expr e1) (tr_expr e2)
    | Read(Str(e, x))   -> Deref(tr_mem e x)
  and tr_mem (e: Asimp.typ Asimp.expression) (x: string): Imp.expression = match e.annot with
    | TStruct str_n -> Binop(Add, tr_expr e, Cst (struct_offset (find_struct str_n p.structs).fields x))
    | _ -> raise(Invalid_argument "Should be a TStruct here")
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
    functions = List.map tr_fdef p.functions }
