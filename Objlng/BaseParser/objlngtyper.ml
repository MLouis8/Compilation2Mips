open Objlng

(* types for various environments *)
module Env = Map.Make(String)
type tenv = typ Env.t
type fenv = function_def Env.t
type cenv = class_def Env.t

(* utility function *)
let check t typ =
  if t <> typ then failwith "type error";
  t

let add2env l env =
  List.fold_left (fun env (x, t) -> Env.add x t env) env l

exception Error_msg of string
(* main typing function *)
let type_program (p: program): unit =

  (* initialize global environments *)
  let tenv = add2env p.globals Env.empty in
  let fenv = add2env (List.map (fun (f: function_def) -> f.name, f) p.functions) Env.empty in
  let cenv = add2env (List.map (fun (c: class_def) -> c.name, c) p.classes) Env.empty in

  let rtrv_func x =
    try Env.find x fenv
    with Not_found -> raise (Error_msg "function not found while calling")
  in

  let rtrv_class x =
    try Env.find x cenv
    with Not_found -> raise (Error_msg "class not found in class environement")
  in

  (* typing a function definition *)
  let type_fdef (fdef: Objlng.function_def) (tenv: tenv): Objlng.typ list =
    (* add local elements to the environments *)
    let tenv = add2env fdef.locals tenv in
    let tenv = add2env fdef.params tenv in

    (* note: nested definitions ensure that all environments are known to the
       inner functions, without making them explicit arguments *)

    (* type expressions *)
    let rec type_expr (e: expression): Objlng.typ = match e with
      | Cst n             -> TInt
      | Bool b            -> TBool
      | Var x             -> Env.find x tenv
      | Binop(op, e1, e2) -> begin match op with
        | Add | Mul -> let _ = check (type_expr e1) TInt in
                       check (type_expr e2) TInt
        | Lt -> let _ = check (type_expr e1) TBool in check (type_expr e2) TBool
      end
      | Call(x, l)        -> let f = (rtrv_func x) in
        List.iter2 (fun arg1 arg2 -> let _ = check (type_expr arg1) (snd arg2) in ()) l f.params; f.return
      | MCall(e, x, l)    -> begin
        match  type_expr e with
          | TClass class_name ->
            let c = rtrv_class (class_name) in
            let met = List.find (fun (m: function_def) -> m.name = x) c.methods in
            let _ = List.iter2 (fun arg param -> let _ = check (type_expr arg) (snd param) in ()) l met.params in met.return
          | _ -> failwith "type error, should be a class here"
        end
      | New(x, l)         -> let _ = rtrv_class x in TClass x
      | NewTab(t, e)      -> let _ = check (type_expr e) TInt in TArray t
      | Read(m)           -> type_mem m
      | This              -> Env.find "_this" tenv
    and type_mem: mem -> Objlng.typ = function
      | Arr(e1, e2) -> begin
        let _ = check (type_expr e2) TInt in
        match type_expr e1 with
        | TArray t -> t
        | _ -> failwith "type error, the expression must be an array"
      end
      | Atr(e, x) ->
        match type_expr e with
        | TClass cname -> begin
          let c = List.find (fun c -> c.name = cname) p.classes in
          try snd (List.find (fun f -> fst f = x) c.fields)
          with Not_found -> failwith "attribute not found"
        end
        | _ -> failwith "type error, the expression must be a class"   
    in

    (* type instructions *)
    let rec type_seq (s: Objlng.sequence): Objlng.typ list = List.map type_instr s
    and type_instr: Objlng.instruction -> Objlng.typ = function
      | Putchar e     -> check (type_expr e) TInt
      | Set(x, e)     -> check (type_expr e) (Env.find x tenv)
      | If(b, s1, s2) -> let _ = type_seq s1 in
                         let _ = type_seq s2 in
                         check (type_expr b) TBool
      | While(e, s)   -> let _ = type_seq s in
                         check (type_expr e) TBool
      | Return e      -> check (type_expr e) fdef.return
      | Expr e        -> type_expr e
      | Write(m, e)   -> check (type_expr e) (type_mem m)
    in
    type_seq fdef.code
    in
    let type_cdef (cdef: class_def): unit =
    let tenv = Env.add "_this" (TClass cdef.name)  tenv in
    List.iter (fun met -> let _ = type_fdef met tenv in ()) cdef.methods
  in
  List.iter (fun f -> let _ = type_fdef f tenv in ()) p.functions;
  List.iter (fun f -> let _ = type_cdef f in ()) p.classes