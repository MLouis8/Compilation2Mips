let tr_op: Objlng.binop -> Imp.binop = function
  | Add -> Add
  | Mul -> Mul
  | Lt  -> Lt

let to_string (expr: Objlng.typ Objlng.expression) = match  expr.expr with
  | Call(_) -> "Call"
  | MCall(_) -> "MCall"
  | New(_) -> "New"
  | Read(_) -> "Read"
  | _ -> "smth else"

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
      | Call(x, l) -> Call(x, List.map tr_expr l)
      | This -> Var ("this")
      | NewTab(t, e) -> Alloc(Binop(Mul, tr_expr e, Cst (typ2byt te.annot)))
      | Read m -> Deref(tr_mem m)
      | _ -> failwith ("Expr not catch: "^to_string te)
    and tr_mem: Objlng.typ Objlng.mem -> Imp.expression = function
      | Atr(e, x) -> Binop(Add, tr_expr e, field_offset x (find_class e.annot))
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
          let call = Imp.Call(x2 ^ "_constructor", Var x1 :: List.map tr_expr l) in (*[Imp.Var x1] @ *)
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
  (* modifying methods in class_definitions:
      - changing the constructor (adding "this" param)
      - changing method name "adding class name"
    *)
  let rectify_methods (cdef: Objlng.typ Objlng.class_def): Objlng.typ Objlng.class_def =
    let mets = List.map (
      fun (met: Objlng.typ Objlng.function_def): Objlng.typ Objlng.function_def ->
        if met.name = "constructor" then
          {
            name=cdef.name^"_"^met.name;
            params=("this", TVoid)::met.params;
            locals=met.locals;
            code=met.code;
            return=met.return;
          }
        else
          {met with name = cdef.name^"_"^met.name}
    ) cdef.methods
    in {cdef with methods=mets}
  in
  (* translating to imp class_definitions methods *)
  let tr_cdef_methods (cdef: Objlng.typ Objlng.class_def): Imp.function_def list =
    List.map tr_fdef (rectify_methods cdef).methods
  in
  (* creating a class descriptor: we choose to make it as a function called in main *)
  let tr_c_descriptor (cdef: Objlng.typ Objlng.class_def): Imp.function_def =
    let descr_name: string = cdef.name^"_descr" in
    let methods =
      List.mapi (
        fun i (met: Objlng.typ Objlng.function_def) -> Imp.Write(Binop(Add, Var descr_name, Cst ((i+1)*4)), Imp.Addr(met.name)))
        (rectify_methods cdef).methods
    in
    let code = [Imp.Set(descr_name, Imp.Alloc(Cst (List.length cdef.methods * 4))); Imp.Write(Var descr_name, Cst 0)] @ methods
    in
    { name=cdef.name^"_descriptor"; params=[]; locals=[]; code=code }
  in
  (* function for addind class descriptors call in main *)
  let tr_functions (fdef: Objlng.typ Objlng.function_def) (c_descrs: Imp.function_def list): Imp.function_def =
    if fdef.name = "main" then
      let seq = List.map (fun (c_descr: Imp.function_def) -> Objlng.Expr({annot=Objlng.TVoid; expr=Objlng.Call(c_descr.name, [])})) c_descrs
      in
      tr_fdef {fdef with code=seq @ fdef.code}
    else
      tr_fdef fdef
  in
  let class_descriptors = List.map tr_c_descriptor p.classes
  in
  { Imp.globals = List.map fst p.globals @ List.map (fun (c: Objlng.typ Objlng.class_def) -> c.name^"_descr") p.classes;
    Imp.functions = class_descriptors @ List.flatten (List.map tr_cdef_methods p.classes) @ List.map (fun f -> tr_functions f class_descriptors) p.functions; }