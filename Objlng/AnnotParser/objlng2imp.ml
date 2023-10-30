let tr_op: Objlng.binop -> Imp.binop = function
  | Add -> Add
  | Mul -> Mul
  | Lt  -> Lt

(* types for various environments *)
module Env = Map.Make(String)

let add2env l env =
  List.fold_left (fun env (x, t) -> Env.add x t env) env l

(* translate a program *)
let translate_program (p: Objlng.typ Objlng.program) =
  let find_class: Objlng.typ -> Objlng.class_def = function
    | Objlng.TClass cname -> List.find (fun (cdef: Objlng.typ Objlng.class_def) -> cdef.name = cname) p.classes
    | _ -> failwith "should already be checked and refused"
  in
  let class_offset cdef: Objlng.typ Objlng.class_def -> Imp.expression = failwith "not implemented" in

  (* translate a function *)
  let tr_fdef (fdef: Objlng.typ Objlng.function_def) (this: string) : Imp.function_def = 
    let rec typ2byt: Objlng.typ -> int = function
      | TInt      -> 4
      | TBool     -> 1
      | TClass x ->
        let def = find_class (TClass x) in
        List.fold_left (fun cpt p -> cpt + typ2byt (snd p)) 0 def.fields
      | TArray t  -> typ2byt t
      | TVoid     -> raise(Invalid_argument "TVoid isn't a valid type")
    in
    (* translation of an expression *)
    let rec tr_expr (te: Objlng.typ Objlng.expression): Imp.expression = match te.expr with
      | Cst n  -> Cst n
      | Bool b -> Bool b
      | Var x  -> Var x
      | Binop(op, e1, e2) -> Binop(tr_op op, tr_expr e1, tr_expr e2)
      | This -> Deref(class_offset (find_class te.annot))
      | NewTab(t, e) -> Alloc(Binop(Mul, tr_expr e, Cst (typ2byt te.annot)))
      | Read m -> tr_mem m
      | _ -> failwith "shoudn't get here"
    and tr_mem: Objlng.typ Objlng.mem -> Imp.expression = function
      | Atr(e, x) ->
        Deref(Binop(Add, tr_expr e, class_offset x (find_class e.annot)))
        | Arr(e1, e2) -> Imp.array_access (tr_expr e1) (tr_expr e2)
    in

    (* translation of instructions *)
    let rec tr_seq s = List.map tr_instr s
    and tr_instr: Objlng.typ Objlng.instruction -> Imp.instruction = function
      | Putchar e     -> Putchar(tr_expr e)
      | If(e, s1, s2) -> If(tr_expr e, tr_seq s1, tr_seq s2)
      | While(e, s) -> While(tr_expr e, tr_seq s)
      | Return e -> Return(tr_expr e)
      | Expr e -> Expr(tr_expr e)
      | Set(x, e) -> match e.expr with
        | New
        | Call
        | DCall
        | _
      | Write(m, e) -> let ma = tr_mem m in Write(ma, tr_expr e)
    in
    { Imp.name = fdef.name; 
    params = List.map fst fdef.params; 
    locals = List.map fst fdef.locals; 
    code = tr_seq fdef.code;
    }
  in
  let tr_cdef (cdef: Objlng.typ Objlng.class_def): Imp.function_def list =
    List.map (fun met -> tr_fdef met cdef.name) cdef.methods
in
  { Imp.globals = List.map fst p.globals;
    functions = List.map (fun f -> tr_fdef f "None") p.functions @ List.flatten (List.map tr_cdef p.classes); }