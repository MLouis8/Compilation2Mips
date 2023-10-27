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
  let find_class name =
    List.find (fun (cdef: Objlng.class_def) -> cdef.name = name) p.classes
  in
  let class_access cdef = failwith "not implemented" in
  let class_offset field cdef = failwith "not implemented" in

  let tr_fdef (fdef: Objlng.function_def) (this: string) : Imp.function_def = 

    (* translation of an expression *)
    let rec tr_expr: Objlng.expression -> Imp.expression = function
      | Cst n  -> Cst n
      | Bool b -> Bool b
      | Var x  -> Var x
      | Binop(op, e1, e2) -> Binop(tr_op op, tr_expr e1, tr_expr e2)
      | This -> class_access (find_class this)
      | NewTab(t, e) -> Alloc(Binop(Mul, tr_expr e, Cst (typ2byt te.annot))) (*here we also need typin*)
      | Read m -> tr_mem m
      | _ -> failwith "Error, tr_instr doesn't take care of special expressions"
    and tr_mem: Objlng.mem -> Imp.expression = function
      | Atr(e, x) -> begin
        let cname = "class" (*here we need typing because TClass cname contains cname*) in
        let offset = class_offset x (find_class cname) in
        Deref(Binop(Add, tr_expr e, offset))
      end
      | Arr(e1, e2) -> Imp.array_access (tr_expr e1) (tr_expr e2)
    in

    (* translation of instructions *)
    let rec tr_seq s = List.map tr_instr s
    and tr_instr: Objlng.instruction -> Imp.instruction = function
      | Putchar e     -> Putchar(tr_expr e)
      | If(e, s1, s2) -> If(tr_expr e, tr_seq s1, tr_seq s2)
      | While(e, s) -> While(tr_expr e, tr_seq s)
      | Return e -> Return(tr_expr e)
      | Expr e -> Expr(tr_expr e)
      | Set(x1, New(x2, l)) -> failwith "not implemented"
      | Set(x1, Call(x2, l)) -> failwith "not implemented"
      | Set(x1, MCall(e, x2, l)) -> failwith "not implemented"
      | Set(x, e) -> failwith "not implemented"
      | Write(m, New(x, l)) -> failwith "not implemented"
      | Write(m, Call(x, l)) -> failwith "not implemented"
      | Write(m, MCall(e, x, l)) -> failwith "not implemented"
      | Write(m, e) -> let ma = tr_mem m in Write(ma, tr_expr e)
    in
    { Imp.name = fdef.name; 
    params = List.map fst fdef.params; 
    locals = List.map fst fdef.locals; 
    code = tr_seq fdef.code;
    }
  in
  let tr_cdef (cdef: Objlng.class_def): Imp.function_def list =
    List.map (fun met -> tr_fdef met cdef.name) cdef.methods
in
  { Imp.globals = List.map fst p.globals;
    functions = List.map (fun f -> tr_fdef f "None") p.functions @ List.flatten (List.map tr_cdef p.classes); }