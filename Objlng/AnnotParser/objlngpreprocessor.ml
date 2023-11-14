open Objlng
open String

module Env = Map.Make(String)
type penv = typ Env.t
type tenv = typ Env.t

let add2env l env = 
  List.fold_left (fun env (x, t) -> Env.add x t env) env l

exception Error_msg of string

(* typing a program *)
let preprocess_program (p: typ program): typ program =

  (* initialize global environments *)
  let tenv = add2env p.globals Env.empty in
  let penv = Env.empty in
  
  (* preprocessing a function definition *)
  let preprocess_fdef (tenv: tenv) (penv: penv) (fdef: typ function_def): typ function_def =
    (* add local elements to the environments *)
    let tenv = add2env fdef.locals tenv in
    let tenv = add2env fdef.params tenv in  
    let locals_ptr = ref fdef.locals in
    let rec preprocess (x: string) (e: typ expression): typ sequence =
      match e.expr with
        | Binop(op, e1, e2) ->
          let t = Env.find x tenv in
          let x1 = x^"_temp1" in
          let x2 = x^"_temp2" in
          let _ = locals_ptr := (x1, t):: !locals_ptr in
          let _ = locals_ptr := (x2, t):: !locals_ptr in
          preprocess x1 e1 @ preprocess x2 e2 @
          [Set(x, mk_expr t (Binop(op, mk_expr t (Var x1), mk_expr t (Var x2))))]
        | MCall(e1, y, l) -> (
          match e1.expr with
            | Var c -> [Set (x, e)]
            | _ -> 
              let x1 = x^"_temp" in
              let _ = locals_ptr := (x1, e1.annot):: !locals_ptr in
              preprocess x1 e1 @ [Set(x, mk_expr e1.annot (MCall(mk_expr e1.annot (Var x1), y, l)))]
        )
        | _ -> [Set (x, e)]
    in
    (* preprocess instructions *)
    let rec preprocess_seq (s: typ Objlng.sequence): typ Objlng.sequence =
      List.fold_left (fun seq instr -> seq @ preprocess_instr instr) [] s
    and preprocess_instr: typ Objlng.instruction -> typ Objlng.instruction list = function
      | Putchar e     -> [Putchar (e)]
      | Set(x, e)     -> preprocess x e
      | If(b, s1, s2) -> [If (b, preprocess_seq s1, preprocess_seq s2)]
      | While(e, s)   -> [While (e, preprocess_seq s)]
      | Return e      -> [Return e]
      | Expr e        -> [Expr e]
      | Write(m, e)   -> [Write(m, e)]
    in
    let code = preprocess_seq fdef.code in
    { fdef with code=code; locals= !locals_ptr }
    in
    let preprocess_cdef (cdef: 'a class_def): typ class_def =
      { cdef with methods = List.map (fun met -> preprocess_fdef tenv penv met) cdef.methods }
  in
  { globals=p.globals;
    functions=List.map (preprocess_fdef tenv penv) p.functions;
    classes=List.map preprocess_cdef p.classes }