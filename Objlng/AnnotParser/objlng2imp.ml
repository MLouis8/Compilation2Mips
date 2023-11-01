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
  let find_class: Objlng.typ -> Objlng.typ Objlng.class_def = function
    | Objlng.TClass cname -> List.find (fun (cdef: Objlng.typ Objlng.class_def) -> cdef.name = cname) p.classes
    | _ -> failwith "should already be checked and refused"
  in
  let field_offset (f: string) (cdef: Objlng.typ Objlng.class_def): Imp.expression =
    let (offset, _) = List.fold_left (
      fun pair field -> if snd pair || fst field = f then (fst pair, true) else (fst pair+4, false)
    ) (4, false) cdef.fields in Cst offset
  in
  let create_instance (cdef: Objlng.typ Objlng.class_def): Imp.expression =
    let sum_fields_size = List.fold_left (fun cpt field -> cpt + 4) 0 cdef.fields in (*try with typ2bit after*)
    Alloc(Binop(Add, Cst 4, Cst sum_fields_size))
  in
  (* translate a function *)
  let tr_fdef (fdef: Objlng.typ Objlng.function_def): Imp.function_def = 
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
      | This -> let TClass cname = te.annot in Var (cname^"_descr")
      | NewTab(t, e) -> Alloc(Binop(Mul, tr_expr e, Cst (typ2byt te.annot)))
      | Read m -> Deref(tr_mem m)
      | _ -> failwith "shoudn't get here"
    and tr_mem: Objlng.typ Objlng.mem -> Imp.expression = function
      | Atr(e, x) -> Binop(Add, Deref(tr_expr e), field_offset x (find_class e.annot))
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
      | Set(x1, e) -> begin match e.expr with
        | New(x2, l) -> let instance = create_instance (find_class (TClass x2)) in
          let call = Imp.Call(x2 ^ "_constructor", List.map tr_expr l) in (*[Imp.Var x1] @ *)
          Seq([Set(x1, instance); Write(Var x1, Var (x2^"_descr")); Expr call])
        | Call(x2, l) -> Set(x1, Call(x2, List.map tr_expr l))
        | MCall(e2, x2, l) -> failwith "not implemented"
        | _ -> Set(x1, tr_expr e)
      end
      | Write(m, e) -> Write(tr_mem m, tr_expr e)
    in
    { Imp.name = fdef.name; 
    params = List.map fst fdef.params; 
    locals = List.map fst fdef.locals; 
    code = tr_seq fdef.code;
    }
  in
  let tr_cdef_methods (cdef: Objlng.typ Objlng.class_def): Imp.function_def list =
    List.map (fun (met: Objlng.typ Objlng.function_def) -> tr_fdef {met with name = cdef.name^"_"^met.name}) cdef.methods
  in
  let tr_c_descriptor (cdef: Objlng.typ Objlng.class_def): Imp.function_def =
    let seq = failwith "not implemented"
    in
    { name=cdef.name; params=[]; locals=[]; code=seq }
  in
  let tr_functions (fdef: Objlng.typ Objlng.function_def) (c_descrs: Imp.function_def list): Imp.function_def =
    if fdef.name = "main" then
      failwith "not implemented"
    else
      tr_fdef fdef
  in
  let class_descriptors = List.map tr_c_descriptor p.classes
  in
  { Imp.globals = List.map fst p.globals @ List.map (fun (c: Objlng.typ Objlng.class_def) -> c.name^"_descr") p.classes;
    Imp.class_descriptors = class_descriptors;
    Imp.functions = List.flatten (List.map tr_cdef_methods p.classes) @ List.map (fun f -> tr_functions f class_descriptors) p.functions; }