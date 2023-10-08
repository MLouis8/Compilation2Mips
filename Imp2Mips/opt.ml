open Imp
open Graph

(* constant propagation optimization *)
let increment cpt n op = match op with
   | Add -> cpt+n
   | Mul -> if cpt=0 then n else cpt*n
   | Lt  -> failwith "not supported"

let partial_eval expr =
   let rec cst_propagation expr cpt = match expr with
      | Cst n -> Cst n
      | Bool b -> Bool b
      | Var x -> Var x
      | Call(x, l) -> Call(x, List.map (fun x -> cst_propagation x 0) l)
      | Binop(Add, Cst n1, Cst n2) -> Cst (n1 + n2 + cpt)
      | Binop(Mul, Cst n1, Cst n2) -> if cpt=0 then Cst (n1 * n2) else Cst(n1 * n2 * cpt)
      | Binop(Lt, Cst n1, Cst n2)  -> Bool (n1 < n2)
      | Binop(op, Cst n, Binop(sub_op, e1, e2)) ->
         let e = Binop(sub_op, e1, e2) in
         let new_cpt = increment cpt n op in
         if op = sub_op then
            let opt = cst_propagation e new_cpt in 
            if opt <> e then opt else Binop(op, Cst new_cpt, opt)
         else Binop(op, Cst new_cpt, cst_propagation e 0)
      | Binop(op, Binop(sub_op, e1, e2), Cst n) ->
         let e = Binop(sub_op, e1, e2) in
         let new_cpt = increment cpt n op in
         if op = sub_op then
            let opt = cst_propagation e new_cpt in 
            if opt <> e then opt else Binop(op, opt, Cst new_cpt)
         else Binop(op, cst_propagation e 0, Cst new_cpt)
      | Binop(op, Cst n, Var x) -> Binop(op, Cst (increment cpt n op), Var x)
      | Binop(op, Var x, Cst n) -> Binop(op, Var x, Cst (increment cpt n op))
      | Binop(op, e1, e2) ->
         let opt1 = cst_propagation e1 cpt in
         let opt2 = if opt1 <> e1 then cst_propagation e2 0 else cst_propagation e2 cpt in
         Binop(op, opt1, opt2)
   in cst_propagation expr 0

(* TODO: Register allocation optimization *)
(* We create a naive interference graph *)
(* module G = Imperative.Graph.Abstract(struct type t = Var end)

let interference_graph fdef =
   let g = G.create () in
   let nodes =
      let new_node id = let v = G.V.create id in G.add_vertex g v; v in
      List.iter (new_node) fdef.params *)
