open Imp
open Nimp

(* sort by ascending lower bound, and sort equals by ascending upper bound *)
let sort_2 l =
  List.stable_sort (fun (_, l1, _) (_, l2, _) -> l1 - l2) l
let sort_3 l =
  List.stable_sort (fun (_, _, h1) (_, _, h2) -> h1 - h2) l
let sort_intervals l =
  sort_2 (sort_3 l)

(* insert interval [i] in active list [l] 
   pre/post-condition: sorted by ascending upper bound *)
let rec insert_active i l = let (_, _, i_bound) = i in
  match l with
  | [] -> [i]
  | (_, _, upper) :: t when upper > i_bound -> i :: l
  | h :: t -> h :: insert_active i t

(* raw allocation information for a variable *)
type raw_alloc =
  | RegN  of int  (* index of the register *)
  | Spill of int  (* index of the spill *)

(* allocation of the local variables of a function [fdef] using linear scan
   algorithm, with [nb_regs] registers available for allocation ;
   return a raw allocation for each variable, as well as the maximum index of
   used registers, and the number of used stack slots (spills) *)
let lscan_alloc nb_regs fdef =
  let live_intervals = Liveness.liveness_intervals_from_liveness fdef in
  let alloc: (string, raw_alloc) Hashtbl.t = Hashtbl.create (List.length fdef.locals+List.length fdef.params) in
  let active = ref [] in
  let free_regs = ref (List.init nb_regs (fun i -> i)) in
  let free_stack = ref [] in
  let r_max = ref (-1) in (* maximum index of used register *)
  let spill_count = ref 0 in (* number of spilled variables *)
  (* free registers allocated to intervals that stop before timestamp a,
     update the free_stack table
     returns remaining intervals *)
  let rec expire a l =
    match l with
    | [] -> []
    | (var, _, upper) :: t when upper < a -> begin
      match Hashtbl.find alloc var with
        | RegN r -> free_regs := r :: !free_regs; expire a t
        | Spill s ->
          free_stack := s :: !free_stack;
          spill_count := !spill_count - 1;
          expire a t
    end
    | h :: t -> h :: expire a t
  in
  (* for each interval i, in sorted order *)
  List.iter (fun i ->
      let xi, li, hi = i in
      (* Printf.printf "interval of: %s, li: %d; hi: %d\n" xi li hi; *)
      (* free registers that expire before the lower bound of i *)
      (* if there are available registers *)
        (* ... then allocate one *)
        (* otherwise, may replace an already used register if this can
           make this register available again earlier *)
      active := expire li live_intervals;
      match !free_regs with
        | [] -> begin
          match !free_stack with
            | [] ->
              Hashtbl.add alloc xi (Spill !spill_count);
              spill_count := !spill_count + 1
            | h :: t ->
              Hashtbl.add alloc xi (Spill h);
              free_stack := t;
              active := insert_active (xi, li, hi) !active;
          end
        | h :: t ->
          Hashtbl.add alloc xi (RegN h);
          free_regs := t;
          active := insert_active (xi, li, hi) !active;
          r_max := if h > !r_max then h else !r_max
    ) (sort_intervals live_intervals);
  (* Hashtbl.iter (fun x raw ->
    match raw with
      | RegN  n -> Printf.printf "Reg(%d) -> %s\n" n x
      | Spill n -> Printf.printf "Spill(%d) -> %s\n" n x
  ) alloc; *)
  alloc, !r_max, !spill_count
