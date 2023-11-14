open Objlng

module Env = Map.Make(String)
type tenv = typ Env.t
type fenv = typ function_def Env.t
type cenv = typ class_def Env.t

let add2env l env = 
  List.fold_left (fun env (x, t) -> Env.add x t env) env l

exception Error_msg of string

(* typing a program *)
let preprocess_program (p: typ program): typ program =

  (* initialize global environments *)
  let tenv = add2env p.globals Env.empty in
  let fenv = add2env (List.map (fun (f: typ function_def) -> f.name, f) p.functions) Env.empty in
  let cenv = add2env (List.map (fun (c: typ class_def) -> c.name, c) p.classes) Env.empty in

  let rtrv_func x =
    try Env.find x fenv
    with Not_found -> raise (Error_msg "function not found while calling")
  in

  let rtrv_class x =
    try Env.find x cenv
    with Not_found -> raise (Error_msg "class not found in class environement")
  in

  let rtrv_c_name: typ -> string = function
    | TClass name -> name
    | _ -> assert false
  in
  (* preprocessing a function definition *)
  let preprocess_fdef (tenv: tenv) (fdef: typ function_def): typ function_def =
    (* add local elements to the environments *)
    let tenv = add2env fdef.locals tenv in
    let tenv = add2env fdef.params tenv in  

    (* search if a method exist in the class_definition or in a parent class_definition and returns the method definition *)
    let find_inherited_met (met_name:string) (cdef: typ class_def): typ function_def =
      let find_met cdef =
        List.find (fun (m: typ function_def) -> m.name = met_name) cdef.methods
      in
      let rec sub_check (met_name: string) (cdef: typ class_def): typ function_def =
        let inheritance = has_get_parent cdef p.classes in 
        if fst inheritance then
          try sub_check met_name (snd inheritance)
          with Not_found -> find_met cdef
        else
          find_met cdef
      in
      try sub_check met_name cdef
      with Not_found -> failwith "method not found"
    in

    (* preprocess expressions *)
    let rec preprocess_expr (e: typ expression): typ expression = match e.expr with
      | Cst n             -> e
      | Bool b            -> e
      | Var x             -> e
      | Binop(op, e1, e2) -> mk_expr e.annot (Binop(op, preprocess_expr e1, preprocess_expr e2))
      | Call(x, l)        -> let f = rtrv_func x in
      let _ = List.iter2 (fun arg1 arg2 -> let _ = preprocess_expr arg1 in ()) l f.params in
      mk_expr e.annot (Call(x, List.map preprocess_expr l))
      | MCall(e, x, l)    ->
        let processed_e = preprocess_expr e in
        let met = find_inherited_met x (rtrv_class (rtrv_c_name processed_e.annot)) in
        let processed_l = List.map2 (fun arg param -> preprocess_expr arg) l met.params in
        mk_expr e.annot (MCall(processed_e, x, processed_l))
      | New(x, l)         -> mk_expr e.annot (New(x, List.map preprocess_expr l))
      | NewTab(t, e)      -> mk_expr e.annot (NewTab(t, preprocess_expr e))
      | Read(m)           -> mk_expr e.annot (Read(preprocess_mem m))
      | This              -> mk_expr (Env.find "_this" tenv) This
    and preprocess_mem: typ mem -> typ mem = function
      | Arr(e1, e2) -> Arr(preprocess_expr e1, preprocess_expr e2)
      | Atr(e, x) -> Atr(preprocess_expr e, x)
    in
    let rec preprocess x e = failwith "not implemented" in
    (* preprocess instructions *)
    let rec preprocess_seq (s: 'a Objlng.sequence): typ Objlng.sequence = List.map preprocess_instr s
    and preprocess_instr: 'a Objlng.instruction -> typ Objlng.instruction = function
      | Putchar e     -> Putchar (preprocess_expr e)
      | Set(x, e)     -> 
        match e.expr with
          | Binop(op, e1, e2) | MCall(e, x, l) | Call(x, l) -> preprocess x e
          | _ -> Set (x, preprocess_expr e)
      | If(b, s1, s2) -> If (preprocess_expr b, preprocess_seq s1, preprocess_seq s2)
      | While(e, s)   -> While (preprocess_expr e, preprocess_seq s)
      | Return e      -> Return (preprocess_expr e)
      | Expr e        -> Expr (preprocess_expr e)
      | Write(m, e)   -> Write(preprocess_mem m, (preprocess_expr e))
    in
    { fdef with code = preprocess_seq fdef.code }
    in
    let preprocess_cdef (cdef: 'a class_def): typ class_def =
      { cdef with methods = List.map (fun met -> preprocess_fdef tenv met) cdef.methods }
  in
  { globals=p.globals;
    functions=List.map (preprocess_fdef Env.empty) p.functions;
    classes=List.map preprocess_cdef p.classes }