(**
   Simple translation from IMP to MIPS.

   Summary of MIPS architecture and assembly language.


   32-bits architecture comprising:
   - an arithmetic and logical unit
   - 32 registers
     * general purpose: $t0-$t9, $s0-$s7, $a0-$a3, $v0-$v1
     * special purpose: $ra, $sp, $fp, $gp, $zero
     (+3 reserved)
   - randomly addressable memory, containing code and data


   A MIPS program contains two parts :
   - instructions, introduced by the directive

       .text

   - data, introduced by the directive

       .data

   The data part contains statically allocated data, for instance the
   global variables of a program. Each piece of data is given a label
   that will be used for accessing the data.



   *** Arithmetic ***

   Arithmetic instructions in "three addresses" format, that apply to 
   values stored in registers, and also put the result in a register.
   Three addresses format:

     add  $r1, $r2, $r3

   means  $r1 <- $r2 + $r3 
   where $r1 is the register where the result is to be stored, and $r2 and
   $r3 are the registers containing the value to be summed.
   
   Many instructions in this format: 
   - arithmetic: add, sub, mul, div, rem, sll, srl
   - logic: and, or
   - comparisons: seq, sne, slt, sle, sgt, sge
   Some unary operations ask for only one operand:
   - not, neg, move

   A few arithmetic instructions take a number as second operand.
   For instance:

     addi  $r1, $r2, i

   for  $r1 <- $r2 + i


   Loading a value in a register:

     li  $r, i

   for  $r <- i


   *** Memory ***

   A memory address is identified by a base address contained in a
   register [$r], and an offset [o] relative to the base address. The
   offset is given directly as a number.
   Notation: o($r).
   Addresses are given in bytes. A 32-bits piece of data occupies 4 bytes.

   Read access

     lw  $r1, o($r2)

   for  $r1 <- *($r2+o)
   which means "$r1 takes the value stored at address $r2+o".

   Write access

     sw  $r1, o($r2)

   for  *($r2+o) <- $r1
   which means "store at address $r2+o the value of register $r1".


   Statically allocated data are, as everything else, stored in memory. 
   Retrieve the address associated to a label [lab] of statically allocated
   data with
   
     la  $r1, lab


   *** Branching instructions ***

   The running program is stored in memory: each instruction has an address.
   Special register [pc] contains the address of the next instruction.

   In most cases, execution of a given instruction is followed by the execution
   of its immediate successor in memory, which corresponds to
     pc <- pc + 4
   Jump or branch instructions drive execution towards other parts of the
   program, identified by one the following means:
   - a label written at some place in the assembly program,
   - an address that has been computed and stored in a register.

   Unconditional jumps with target given by a label 
   (two versions are subtly different, but we will ignore the difference).
     
     j  lab
     b  lab

   Unconditional jump with target address given by a register 

     jr  $r

   Two variants used for function calls, which store the value [pc+4] in the
   special purpose register $ra. This stored address identifies the instruction
   where execution should resume once the call is completed.

     jal   lab
     jalr  $r


   Conditional jumps to a label, depending on the result of a test.
   Example: jumps to the instruction with label [lab] si the values in 
   registers $r1 and $r2 are equal.

     beq  $r1, $r2, lab

   Available binary tests:
   - beq, bne, bgt, bge, blt, ble

   Particular cases with only one operand, equivalant to the previous 
   instructions with $r2=0

     beqz  $r1, lab

   Available tests-to-zero:
   - beqz, bnez, bgtz, bgez, bltz, blez


   *** System calls ***

   For this course we do not use actual MIPS hardware but a simulator.
   This simulator includes a few special operations that mimick some services
   that would otherwise be offered by the operating system, or a low-level
   library like the libc. These operations are triggered by the additional
   instruction
     
     syscall

   which is not part of the actual assembly language.
   Each service has a numeric code, that has to be put in register $v0.
   If an argument is needed, it is put in register $a0.

   Some services:
   - code 1: prints the integer contained in register $a0
   - code 10: halts the program
   - code 11: prints the character whose ASCII code is given by $a0
 *)

(**
   Module Imp defines the abstract syntax for the source language.
   Module Mips defines caml functions for generating MIPS instructions.
 *)
open Imp
open Mips

   
(**
   The upper part of the memory (the highest addresses) is used as a stack
   for storing various elements. Special purpose register $sp points to the
   top of the stack (more precisely: the address of the element at the top
   of the stack). The stack grows towards decreasing addresses.
   We define two auxiliary functions [push] and [pop] generating MIPS code
   for adding or removing an element at the top of the stack.
 *)
(* To save the (4-byte) value of a register [reg], decrement $sp by 4 bytes,
   then write the value at the new address pointed by $sp.
   Note: operator @@ concatenates MIPS fragments (defined in Mips module). *)
let push reg =
  subi sp sp 4
  @@ sw reg 0 sp

let pop  reg =
  lw reg 0 sp
  @@ addi sp sp 4

let tr_function (fdef: function_def) =
  let env = Hashtbl.create 16 in
  List.iteri (fun k id -> Hashtbl.add env id (4*(k+1))) fdef.params;
  List.iteri (fun k id -> Hashtbl.add env id (-4*(k+2))) fdef.locals;

  let rec tr_expr = function
    | Cst(n)  -> li t0 n
    | Bool(b) -> if b then li t0 1 else li t0 0
    | Var(id) -> begin
        match Hashtbl.find_opt env id with
        | Some offset -> lw t0 offset fp
        | None -> la t0 id @@ lw t0 0 t0
      end
              
    | Binop(bop, e1, e2) ->
      let op = match bop with
        | Add -> add
        | Mul -> mul
        | Lt  -> slt
      in
      tr_expr e2
      @@ push t0
      @@ tr_expr e1
      @@ pop t1
      (* Apply the binary operation to $t0 (the value of [e1]) and $t1 (the
          value of [e2]), and put the result in $t0. *)
      @@ op t0 t0 t1      
    | Call(f, params) ->
      (* Evaluate the arguments and pass them on the stack. *)
      let params_code =
        List.fold_right
          (fun e code -> code @@ tr_expr e @@ push t0)
          params nop
      in
      params_code (* STEP 1 *)
      @@ jal f
      @@ addi sp sp (4 * List.length params) (* STEP 4 *)

    | Deref e ->
      tr_expr e  (* pointer in t0 *)
      @@ lw t0 0(t0)

    | Alloc e -> (* request e bytes above the heap *)
      tr_expr e
      @@ move a0 t0
      @@ li v0 9
      @@ syscall   (* sbrk -> shifts the limit of the heap *)
      @@ move t0 v0  (* v0 contains the first address of the allocated space *)
    | Addr x -> (* load address x, global function address *)
      la t0 x
    | DCall(e, args) -> (*  *)
      tr_expr e @@
      let params_code =
        List.fold_right
          (fun e code -> code @@ tr_expr e @@ push t1)
          args nop
      in
      params_code

  in
  
  let new_label =
    let cpt = ref (-1) in
    fun () -> incr cpt; Printf.sprintf "__%s_%i" fdef.name !cpt
  in
 
  let rec tr_seq = function
    | []   -> nop
    | [i]  -> tr_instr i
    | i::s -> tr_instr i @@ tr_seq s
 
  and tr_instr = function
    | Putchar(e) ->
       tr_expr e
       @@ move a0 t0
       @@ li v0 11
       @@ syscall
    | Set(id, e) ->
       let set_code = match Hashtbl.find_opt env id with
         | Some offset -> sw t0 offset fp
         | None -> la t1 id @@ sw t0 0 t1
       in
       tr_expr e @@ set_code
 
    (* Conditional *)
    | If(c, s1, s2) ->
       let then_label = new_label()
       and end_label = new_label()
       in
       tr_expr c
       @@ bnez t0 then_label
       @@ tr_seq s2
       @@ b end_label
       @@ label then_label
       @@ tr_seq s1
       @@ label end_label
 
    (* Loop *)
    | While(c, s) ->
       let test_label = new_label()
       and code_label = new_label()
       in
       b test_label
       @@ label code_label
       @@ tr_seq s
       @@ label test_label
       @@ tr_expr c
       (* If the condition is non-zero, jumps back to the beginning of the loop
          body. *)
       @@ bnez t0 code_label
       
    (* Function termination. *)
    | Return(e) ->
       tr_expr e
       @@ addi sp fp (-4)
       @@ pop ra
       @@ pop fp
       @@ jr ra
    | Expr(e) ->
       tr_expr e
 
    | Write(e1, e2) ->
       tr_expr e1  (* t0: pointer *)
       @@ push t0
       @@ tr_expr e2  (* t0: value to be written *)
       @@ pop t1
       @@ sw t0 0(t1)
    | Seq s -> tr_seq s
            
  in
  push fp  (* STEP 2 *)
  @@ push ra
  @@ addi fp sp 4
  @@ addi sp sp (-4 * List.length fdef.locals)   (* END STEP 2 *)
  @@ tr_seq fdef.code
  @@ li t0 0      (* STEP 3 *)
  @@ addi sp fp (-4)
  @@ pop ra
  @@ pop fp
  @@ jr ra     (* END STEP 3 *)

(**
   Main function for translating a program.
 *)
let translate_program prog =
  let init =
    beqz a0 "init_end"
    @@ lw a0 0 a1
    @@ jal "atoi"
    @@ label "init_end"
    @@ push v0
    @@ jal "main"
    @@ li v0 10
    @@ syscall
  and built_ins =
    (* Conversion function string -> int, that iterates on the characters of
       the string. *)
    comment "built-in atoi"
    @@ label "atoi"
    @@ li   v0 0
    @@ label "atoi_loop"
    @@ lbu  t0 0 a0
    @@ beqz t0 "atoi_end"
    @@ addi t0 t0 (-48)
    @@ bltz t0 "atoi_error"
    @@ bgei t0 10 "atoi_error"
    @@ muli v0 v0 10
    @@ add  v0 v0 t0
    @@ addi a0 a0 1
    @@ b "atoi_loop"
    @@ label "atoi_error"
    @@ li   v0 10
    @@ syscall
    @@ label "atoi_end"
    @@ jr   ra
  in

  (**
     Main code for producing the MIPS assembly program corresponding to the
     source IMP program.
   *)
  let function_codes = List.fold_right
    (fun (fdef: function_def) code ->
      label fdef.name @@ tr_function fdef @@ code)
    prog.functions nop
  in
  let text = init @@ function_codes @@ built_ins
  and data = List.fold_right
    (fun id code -> label id @@ dword [0] @@ code)
    prog.globals nop
  in
  { text; data}
