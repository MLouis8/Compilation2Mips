(**
   Abstract syntax for the IMP language.
   Definition of types and data structures to represent an IMP program
   as caml data.
 *)

(**
   Binary operators: +, *, <
 *)
type binop = Add | Mul | Lt

(**
   Data structure for expressions
 *)
type expression =
  (* Integer constant: 0, -1, 42, ... *)
  | Cst   of int
  (* Boolean constant: true, false *)
  | Bool  of bool
  (* Variable, identified by a name *)
  | Var   of string
  (* Binary operation, with an operator and two operands *)
  | Binop of binop * expression * expression
  (* Function call, with a function name and a list of parameters *)
  | Call  of string * expression list
  (* Dereference a pointer *)
  | Deref of expression   (*   *e   *)
  (* Allocate some memory *)
  | Alloc of expression
  (* Objlng extension *)
  | Addr of string (* Addr(name of a statically allocated element) -> returns its adress *)
  | DCall of expression * expression list (* dynamic call: pointer * args list *)
(**
   An expression:
     (1 + x) * f(3, true)
 *)
let e = Binop(Mul,
              Binop(Add, Cst 1, Var "x"),
              Call("f", [Cst 3; Bool true]))

let array_offset t i = Binop(Add, t, Binop(Mul, Cst 4, i))
let array_access t i = array_offset t i

(**
   Data structure for instructions
*)
type instruction =
  (* Primitive operation for printing a char, given as ASCII code *)
  | Putchar of expression
  (* Assignment of a new value to a variable *)
  | Set     of string * expression
  (* Conditional *)
  | If      of expression * sequence * sequence
  (* Loop *)
  | While   of expression * sequence
  (* Function termination *)
  | Return  of expression
  (* Expression used as an instruction (typically function call) *)
  | Expr    of expression
  (* writing in memory *)
  | Write   of expression * expression (*   *e1 = e2;   *)
  (* Objlng extension*)
  | Seq of sequence
(* Instruction sequence *)
and sequence = instruction list
(**
   An instruction:
     while (c < 58) {
       putchar(c);
       c = c+1;
     }
 *)
let i = While(Binop(Lt, Var "c", Cst 58),
              [ Putchar(Var "c");
                Set("c", Binop(Add, Var "x", Cst 1)) ]
          )


let array_write t i e =
  Write(array_offset t i, e)

(** 
   Data structure for a function definition
 *)
type function_def = {
    (* Function name *)
    name: string;
    (* List of named parameters *)
    params: string list;
    (* List of named local variables *)
    locals: string list;
    (* The actual code *)
    code: sequence;
  }
(**
   A function:
     function digits(start) {
       var c;
       c = start + 48;
       while (c < 58) {
         putchar(c);
         c = c+1;
       }
     }
 *)
let f = {
    name = "digits";
    params = ["start"];
    locals = ["c"];
    code = [ Set("c", Binop(Add, Var "start", Cst 48)); i ]
  }

type class_descriptor = {
  name: string;
  methods: string list;
}
(**
   Data structure for a program
 *)
type program = {
    (* List of named global variables *)
    globals: string list;
    (* Class descriptors creation *)
    class_descriptors: class_descriptor list;
    (* The functions defined by the program *)
    functions: function_def list;
  }
(**
   A programme:
     var zero;

     function main() {
       zero = 0;
       digit(zero);
     }

     function digits(start) {
       var c;
       c = start + 48;
       while (c < 58) {
         putchar(c);
         c = c+1;
       }
     }
 *)
let p = {
    globals = ["zero"];
    class_descriptors = [];
    functions = [ f;
                  { name = "main";
                    params = [];
                    locals = [];
                    code = [ Set("zero", Cst 0);
                             Expr(Call("digits", [Var "zero"])) ] } ]
  }
