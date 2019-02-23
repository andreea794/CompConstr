type instr =
  | Lookup of string
  | Push_int of int
  | Plus
  | Times
  | Push_bool of bool
  | Not
  | And
  | Or
  | Less_than
  | Greater_than
  | Equals
  | Assign of string
  | If of instr list * instr list
  | While of instr list * instr list
  | Pop_env
  | Pop_stack
  | Push_closure of compiled_closure
  | Call
and compiled_closure = {
  arg : string;
  body : instr list;
}
;;

val compile_arith : Ast0.arith_exp -> instr list
val compile_bool : Ast0.bool_exp -> instr list
val compile_statement : Ast0.statement -> instr list

type value =
  | Int of int
  | Bool of bool
  | Closure of compiled_closure

type env = (string * (int, compiled_closure) Either.t) list

val lookup : env -> string -> value

exception Malformed_stack

type stack = value list

val one_bool : stack -> bool * stack
val two_bools : stack -> bool * bool * stack

val one_int : stack -> int * stack
val two_ints : stack -> int * int * stack

val interp : env * stack -> instr list -> env * stack

val eval : Ast0.statement -> env * stack
