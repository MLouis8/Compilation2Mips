open Asimp

(* types for various environments *)
module Env = Map.Make(String)
type tenv = typ Env.t
type fenv = unit function_def Env.t
type senv = struct_def Env.t

(* utility function *)
let check te t =
  if te.annot <> t then failwith "type error";
  te

let add2env l env =
  List.fold_left (fun env (x, t) -> Env.add x t env) env l

exception Error_msg of string
(* main typing function *)
let type_program (p: unit program): typ program =

  (* initialize global environments *)
  let tenv = add2env p.globals Env.empty in
  let fenv = add2env (List.map (fun (f: unit function_def) -> f.name, f) p.functions) Env.empty in
  let senv = add2env (List.map (fun s -> s.name, s) p.structs) Env.empty in

  let rtrv_func_type x =
    try let f = Env.find x fenv in f.return
    with Not_found -> raise (Error_msg "function return type not found while calling")
  in

  (* typing a function definition *)
  let type_fdef fdef =
    (* add local elements to the environments *)
    let tenv = add2env fdef.locals tenv in
    let tenv = add2env fdef.params tenv in

    (* note: nested definitions ensure that all environments are known to the
       inner functions, without making them explicit arguments *)

    (* type expressions *)
    let rec type_expr (e: unit expression): typ expression = match e.expr with
      | Cst n             -> mk_expr TInt (Cst n)
      | Bool b            -> mk_expr TBool (Bool b)
      | Var x             -> mk_expr (Env.find x tenv) (Var x)
      | Binop(op, e1, e2) -> let return_t = match op with | Add | Mul -> TInt | Lt -> TBool in 
                             mk_expr return_t (Binop(op, check (type_expr e1) TInt, check (type_expr e2) TInt))
      | Call(x, l)        -> mk_expr (rtrv_func_type x) (Call(x, List.map type_expr l))
      | New x             -> mk_expr (TStruct x) (New x)
      | NewTab(t, e)      -> mk_expr (TArray t) (NewTab(t, check (type_expr e) TInt))
      | Read(m)           -> let (t1, t2) = type_mem m in mk_expr t1 (Read(t2))
      (* | Read(Arr(e1, e2)) -> let t1 = type_expr e1 in begin
                             match t1.annot with
                              | TArray t -> mk_expr t (Read(Arr(t1, check (type_expr e2) TInt)))
                              | _        -> failwith "type error" end
      | Read(Str(e, x))   -> let t1 = type_expr e in begin
                             match t1.annot with
                              | TStruct stru -> 
                                let (_, t) = List.find (fun (str, _) -> str = x) (Env.find stru senv).fields in
                                mk_expr t (Read(Str(t1, x)))
                              | _ -> failwith "type error" end *)
    and type_mem: unit mem -> typ * typ mem = function
      | Arr(e1, e2) -> 
        let t1 = type_expr e1 in begin
          match t1.annot with
          | TArray t -> (t, Arr(t1, check (type_expr e2) TInt))
          | _        -> failwith "type error" end
      | Str(e, x)   ->
        let t1 = type_expr e in begin
          match t1.annot with
           | TStruct stru -> 
             let (_, t) = List.find (fun (str, _) -> str = x) (Env.find stru senv).fields in (t, Str(t1, x))
           | _ -> failwith "type error" end
    in

    (* type instructions *)
    let rec type_seq s = List.map type_instr s
    and type_instr = function
      | Putchar e     -> Putchar (check (type_expr e) TInt)
      | Set(x, e)     -> Set (x, (check (type_expr e) (Env.find x tenv)))
      | If(b, s1, s2) -> If (check (type_expr b) TBool, type_seq s1, type_seq s2)
      | While(e, s)   -> While (check (type_expr e) TBool, type_seq s)
      | Return e      -> Return (check (type_expr e) fdef.return)
      | Expr e        -> Expr (type_expr e)
      | Write(m, e)   -> let t, tm = type_mem m in Write(tm, (check (type_expr e) t))
    in
    { fdef with code = type_seq fdef.code }
  in
  { p with functions = List.map type_fdef p.functions }
