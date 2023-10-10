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

(* main typing function *)
let type_program (p: unit program): typ program =

  (* initialize global environments *)
  let tenv = add2env p.globals Env.empty in
  let fenv = add2env (List.map (fun (f: unit function_def) -> f.name, f) p.functions) Env.empty in
  let senv = add2env (List.map (fun s -> s.name, s) p.structs) Env.empty in

  let rtrv_func_type x =
    let f = Env.find x fenv in f.return
  in

  (* typing a function definition *)
  let type_fdef fdef =
    (* add local elements to the environments *)
    let tenv = add2env fdef.locals tenv in

    (* note: nested definitions ensure that all environments are known to the
       inner functions, without making them explicit arguments *)

    (* type expressions *)
    let rec type_expr (e: unit expression): typ expression = match e.expr with
      | Cst n             -> mk_expr TInt (Cst n)
      | Bool b            -> mk_expr TBool (Bool b)
      | Var x             -> mk_expr (Env.find x tenv) (Var x)
      | Binop(op, e1, e2) -> mk_expr TInt (Binop(op, check (type_expr e1) TInt, check (type_expr e2) TInt))
      | Call(x, l)        -> mk_expr (rtrv_func_type x) (Call(x, List.map type_expr l))
      | New x             -> mk_expr (TStruct x) (New x)
      | NewTab(t, e)      -> mk_expr (TArray t) (NewTab(t, check (type_expr e) TInt))
      | Read(Arr(e1, e2)) -> let t1 = type_expr e1 in
                             let t = match t1.annot with
                              | TArray t -> t
                              | _ -> failwith "type error"
                             in mk_expr t (Read(Arr(t1, check (type_expr e2) TInt)))
      | Read(Str(e, x))   -> let t = type_expr e in
                             let x = match t.annot with
                              | TStruct x -> x
                              | _ -> failwith "type error"
                             in mk_expr (List.find (fun s, t -> if x = s then t) (Env.find x senv).fields)
    and type_mem (m: unit mem): typ mem = TVoid
    in

    (* type instructions *)
    let rec type_seq s = List.map type_instr s
    and type_instr = function
      | Putchar e     -> Putchar (check (type_expr e) TInt)
      | Set(x, e)     -> Set (x, (check (type_expr e) (Env.find x tenv)))
      | If(b, s1, s2) -> If (check (type_expr b) TBool, type_seq s1, type_seq s2)
      | While(e, s)   -> While (check (type_expr e) TBool, type_seq s)
      | Return e      -> Return (type_expr e)
      | Expr e        -> Expr (type_expr e)
      | Write(m, e)   -> Write(type_mem m, type_expr e)
    in
    { fdef with code = type_seq fdef.code }
  in
  { p with functions = List.map type_fdef p.functions }
