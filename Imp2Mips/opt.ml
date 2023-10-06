open Imp
open Graph

let adder x y = x+y
let multiplier x y = x*y
let special x y = x

(* constant propagation optimization *)
let rec partial_eval expr = match expr with
   | Cst n -> Cst n
   | Bool b -> Bool b
   | Var x -> Var x
   | Call(x, l) -> Call(x, List.map partial_eval l)
   | Binop(op, e1, e2) ->
      (* recursive calls *)
      let opt1 = partial_eval e1 in
      let opt2 = partial_eval e2 in
      let cmpt = match op with
         | Add -> adder
         | Mul -> multiplier
         | Lt  -> special
      in
      (* boolean pre evaluation *)
      if op = Lt then match opt1, opt2 with
         | (Bool b1, Bool b2) -> Bool (b1 < b2)
         | (Cst n1, Cst n2)   -> Bool (n1 < n2)
         | _ -> Binop(op, opt1, opt2)
      (* constant propagation *)
      else match opt1, opt2 with
         | (Cst n1, Cst n2) -> Cst (cmpt n1 n2)
         (* Case: Binop(Binop, Cst) or Binop(Cst, Binop) *)
         | (Binop(sub_op, sub_e1, sub_e2), Cst n) | (Cst n, Binop(sub_op, sub_e1, sub_e2)) ->
         begin
            if op = sub_op then match sub_e1 with
            | Cst n1 -> Binop(op, Cst (cmpt n n1), sub_e2) | _ -> match sub_e2 with
            | Cst n2 -> Binop(op, sub_e1, Cst (cmpt n n2)) | _ -> Binop(op, opt1, opt2) 
            else Binop(op, opt1, opt2)
         end
         (* Case: Binop(Binop, Binop)*)
         | (Binop(sub1_op, sub1_e1, sub1_e2), Binop(sub2_op, sub2_e1, sub2_e2)) ->
         begin
            if (sub1_op = op) && (sub2_op = op)
            then
               match sub1_e1 with
               | Cst n1 -> begin
                  match sub2_e1 with
                  | Cst n2 -> Binop(op, Cst (cmpt n1 n2), Binop(op, sub1_e2, sub2_e2))
                  | _ -> begin
                     match sub2_e2 with
                     | Cst n2 -> Binop(op, Cst (cmpt n1 n2), Binop(op, sub1_e2, sub2_e1))
                     | _ -> Binop(op, opt1, opt2)
                  end
               end
               | _ -> begin
                  match sub1_e2 with
                  | Cst n1 -> begin
                     match sub2_e1 with
                     | Cst n2 -> Binop(op, Cst (cmpt n1 n2), Binop(op, sub1_e1, sub2_e2))
                     | _ -> begin
                        match sub2_e2 with
                        | Cst n2 -> Binop(op, Cst (cmpt n1 n2), Binop(op, sub1_e1, sub2_e1))
                        | _ -> Binop(op, opt1, opt2)
                     end
                  end
                  | _ -> Binop(op, opt1, opt2)
               end
            else Binop(op, opt1, opt2)
         end
         (* don't optimize othe cases *)
         | _ -> Binop(op, opt1, opt2)

(* Register allocation optimization *)
(* We create a naive interference graph *)
(* module G = Imperative.Graph.Abstract(struct type t = Var end)

let interference_graph fdef =
   let g = G.create () in
   let nodes =
      let new_node id = let v = G.V.create id in G.add_vertex g v; v in
      List.iter (new_node) fdef.params *)
