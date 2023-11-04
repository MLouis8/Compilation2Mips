open Objlng

(* types for various environments *)
module Env = Map.Make(String)
type tenv = typ Env.t
type fenv = unit function_def Env.t
type cenv = unit class_def Env.t

(* utility function *)
let check te t =
  if te.annot <> t then failwith ("type error: "^typ_to_string te.annot^" is different from "^typ_to_string t);
  te

let add2env l env = 
  List.fold_left (fun env (x, t) -> Env.add x t env) env l

exception Error_msg of string

(* typing a program *)
let type_program (p: 'a program): typ program =

  (* initialize global environments *)
  let tenv = add2env p.globals Env.empty in
  let fenv = add2env (List.map (fun (f: unit function_def) -> f.name, f) p.functions) Env.empty in
  let cenv = add2env (List.map (fun (c: unit class_def) -> c.name, c) p.classes) Env.empty in

  let rtrv_func x =
    try Env.find x fenv
    with Not_found -> raise (Error_msg "function not found while calling")
  in

  let rtrv_class x =
    try Env.find x cenv
    with Not_found -> raise (Error_msg "class not found in class environement")
  in

  (* typing a function definition *)
  let type_fdef (tenv: tenv) (fdef: 'a function_def): typ function_def =
    (* add local elements to the environments *)
    let tenv = add2env fdef.locals tenv in
    let tenv = add2env fdef.params tenv in

    (* checks if the attribute exists in the class_definition or in a parent class_defition and returns the attribute type *)
    let check_inherited_atr (attr: string) (cdef: unit class_def): typ =
      let find_attr cdef =
        try snd (List.find (fun f -> fst f = attr) cdef.fields) 
        with Not_found -> TVoid
      in
      let rec sub_check (attr: string) (cdef: unit class_def): typ =
        let inheritance = has_get_parent cdef p.classes in 
        if fst inheritance then
          match sub_check attr (snd inheritance) with
          | TVoid -> find_attr cdef
          | t     -> t
        else
          find_attr cdef
      in
      let res = try sub_check attr cdef with Not_found -> failwith "attribute not found" in 
      if res = TVoid then failwith "attribute not found in parent" else res
    in

    (* search if a method exist in the class_definition or in a parent class_definition and returns the method definition *)
    let find_inherited_met (met_name:string) (cdef: unit class_def): unit function_def =
      let find_met cdef =
        List.find (fun (m: unit function_def) -> m.name = met_name) cdef.methods
      in
      let rec sub_check (met_name: string) (cdef: unit class_def): unit function_def =
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

    (* type expressions *)
    let rec type_expr (e: 'a expression): typ expression = match e.expr with
      | Cst n             -> mk_expr TInt (Cst n)
      | Bool b            -> mk_expr TBool (Bool b)
      | Var x             -> mk_expr (Env.find x tenv) (Var x)
      | Binop(op, e1, e2) -> let return_t = match op with | Add | Mul -> TInt | Lt -> TBool in 
                              mk_expr return_t (Binop(op, check (type_expr e1) TInt, check (type_expr e2) TInt))
      | Call(x, l)        -> let f = rtrv_func x in
      let _ = List.iter2 (fun arg1 arg2 -> let _ = check (type_expr arg1) (snd arg2) in ()) l f.params in
      mk_expr f.return (Call(x, List.map type_expr l))
      | MCall(e, x, l)    -> begin
        let typed_e = type_expr e in
        match  typed_e.annot with
          | TClass class_name ->
            let met = find_inherited_met x (rtrv_class (class_name)) in
            let typed_l = List.map2 (fun arg param -> check (type_expr arg) (snd param)) l met.params in
            mk_expr met.return (MCall(typed_e, x, typed_l))
          | _ -> failwith "type error, should be a class here"
        end
      | New(x, l)         -> mk_expr (TClass x) (New(x, List.map type_expr l))
      | NewTab(t, e)      -> mk_expr (TArray t) (NewTab(t, check (type_expr e) TInt))
      | Read(m)           -> let (t1, t2) = type_mem m in mk_expr t1 (Read(t2))
      | This              -> mk_expr (Env.find "_this" tenv) This
    and type_mem: 'a mem -> typ * typ mem = function
      | Arr(e1, e2) -> 
        let t1 = type_expr e1 in begin
          match t1.annot with
          | TArray t -> (t, Arr(t1, check (type_expr e2) TInt))
          | _        -> failwith "type error, should be an array here" end
      | Atr(e, x) ->
        let t = type_expr e in
        match t.annot with
        | TClass cname -> begin
          let cdef = find_class (TClass cname) p.classes in
          (check_inherited_atr x cdef, Atr(t, x))
        end
        | _ -> failwith "type error, the expression must be a class"  
    in

    (* type instructions *)
    let rec type_seq (s: 'a Objlng.sequence): typ Objlng.sequence = List.map type_instr s
    and type_instr: 'a Objlng.instruction -> typ Objlng.instruction = function
      | Putchar e     -> Putchar (check (type_expr e) TInt)
      | Set(x, e)     -> Set (x, check (type_expr e) (Env.find x tenv))
      | If(b, s1, s2) -> If (check (type_expr b) TBool, type_seq s1, type_seq s2)
      | While(e, s)   -> While (check (type_expr e) TBool, type_seq s)
      | Return e      -> Return (check (type_expr e) fdef.return)
      | Expr e        -> Expr (type_expr e)
      | Write(m, e)   -> let t, tm = type_mem m in Write(tm, (check (type_expr e) t))
    in
    { fdef with code = type_seq fdef.code }
    in
    let type_cdef (cdef: 'a class_def): typ class_def =
    let tenv = Env.add "_this" (TClass cdef.name)  tenv in
    { cdef with methods = List.map (fun met -> type_fdef tenv met) cdef.methods }
  in
  { globals=p.globals;
    functions=List.map (type_fdef Env.empty) p.functions;
    classes=List.map type_cdef p.classes }