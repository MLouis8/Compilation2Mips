open Imp
open Nimp

module VSet = Set.Make(String)

(* returns the set of variables accessed by the expression [e] *)
let rec use_expr (e: expression): VSet.t = match e with
  | Cst _  -> VSet.empty
  | Bool _ -> VSet.empty
  | Var x  -> VSet.add x VSet.empty
  | Binop(_, e1, e2) -> VSet.union (use_expr e1) (use_expr e2)
  | Call(x, l) ->
    List.fold_left (fun acc arg -> VSet.union acc (use_expr arg)) VSet.empty l

let liveness fdef =
  let n = max_instr_list fdef.code in
  let live = Array.make (n+1) VSet.empty in
  (* returns the set of variable that live in entry to the numbered 
     instruction [i], assuming a set of live variables [lv_out] on 
     exit of [i] *)
  let rec lv_in_instr i lv_out = match i.instr with
    | Putchar e     -> VSet.union (use_expr e) lv_out
    | Set (x, e)    -> VSet.union (use_expr e) (VSet.remove x lv_out)
    | If (e, s1, s2)->
      VSet.union (use_expr e) (VSet.union (lv_in_list s1 lv_out) (lv_in_list s2 lv_out))
    | While (e, s)  -> VSet.union (use_expr e) (lv_in_list s (lv_in_list s lv_out))
    | Return e      -> use_expr e
    | Expr e        -> VSet.union (use_expr e) lv_out
  (* the same for a sequence, and records in [live] the live sets computed
     on entry to each analyzed instruction *)
  and lv_in_list l lv_out = 
    List.fold_right (
      fun instr out -> let alive = lv_in_instr instr out in live.(instr.nb) <- alive; alive
    ) l VSet.empty
  in
  let _ = lv_in_list fdef.code VSet.empty in
  live

let liveness_intervals_from_liveness fdef =
  let live = liveness fdef in
  (* for each variable [x], create the smallest interval that contains all
     the numbers of instructions where [x] is live *)
  let liveness_interval (var: string) (live: VSet.t array): int*int =
    let (_, lower_b, higher_b) = (Array.fold_left (
      fun (id, lower_b, higher_b: int*int*int) (instr_set: VSet.t) ->
        if VSet.exists (fun x -> x = var) instr_set then
          if higher_b = Array.length live then (id-1, lower_b, id)
          else (id-1, lower_b, higher_b)
        else if lower_b = 0
          then (id-1, id, higher_b)
          else (id-1, lower_b, higher_b)
    ) (Array.length live-1, 0, Array.length live) live)
    in (lower_b, higher_b)
  in
  let variables = fdef.params @ fdef.locals in
  List.map (fun x -> let interval = liveness_interval x live in (x, fst interval, snd interval)) variables 
