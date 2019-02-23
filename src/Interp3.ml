[@@@ocaml.warning "-27"]

open Ast
;;

type address = int
;;

type label = string
;;

type location = label * (address option)
;;

type instr =
  | Lookup of string
  | Push_unit
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
  | Goto of location
  | Label of label
  | Case of location
  | Test of location
  | Halt
  | Pop
and compiled_closure = {
  arg : string;
  body : instr list;
}
;;

let new_label = 
    let i = ref 0 in 
    let get () = let v = !i in (i := (!i) + 1; "L"^ (Int.to_string v))
    in get

let rec comp_arith = function
  | Var var -> ([], [Lookup var])
  | Int int -> ([], [Push_int int])
  | Plus (a, b) -> 
    let (defs1, c1) = comp_arith a in
    let (defs2, c2) = comp_arith b in
    (defs1 @ defs2, c1 @ c2, @ [Plus])
  | Times (a, b) ->
    let (defs1, c1) = comp_arith a in
    let (defs2, c2) = comp_arith b in
    (defs1 @ defs2, c1 @ c2, @ [Times])
;;

let rec comp_bool = function
  | True -> ([], [Push_bool true])
  | False -> ([], [Push_bool false])
  | Not exp ->
    let (defs, c) = comp_bool exp in (defs, c @ [Not])
  | And (first, second) ->
    let (defs1, c1) = comp_bool first in
    let (defs2, c2) = comp_bool second in
    (defs1 @ defs2, c1 @ c2 @ [And])
  | Or (first, second) ->
    let (defs1, c1) = comp_bool first in
    let (defs2, c2) = comp_bool second in
    (defs1 @ defs2, c1 @ c2 @ [Or])
  | Bool_op (first, op, second) ->
    let (defs1, c1) = comp_arith first in
    let (defs2, c2) = comp_arith second in
    (match op with
      | Lt -> (defs1 @ defs2, c1 @ c2 @ [Less_than])
      | Gt -> (defs1 @ defs2, c1 @ c2 @ [Greater_than])
      | Eq -> (defs1 @ defs2, c1 @ c2 @ [Equals]))

let rec comp_statement = function
  | Skip -> ([], [])
  | Assign(var, exp) ->
    let (defs, c) = comp_statement exp in
    (defs, c @ [Assign var])
  | Seq(first, second) -> 
    let (defs1, c1) = comp_statement first in
    let (defs2, c2) = comp_statement second in
    (defs1 @ defs2, c1 @ [Pop] @ c2)
  | If(cond, true_, false_) ->
    let else_label = new_label () in 
    let after_else_label = new_label () in 
    let (defs1, c1) = comp cond in  
    let (defs2, c2) = comp true_ in  
    let (defs3, c3) = comp false_ in  
      (defs1 @ defs2 @ defs3, 
      (c1 @ [Test(else_label, None)] @
       c2 @ [Goto(after_else_label, None); Label else_label] @
       c3 @ [Label after_else_label]))
  | While (cond, body) ->
    let test_label = new_label () in 
    let end_label = new_label () in 
    let (defs1, c1) = comp cond in  
    let (defs2, c2) = comp body in  
    (defs1 @ defs2,
    [Label test_label]
     @ c1
     @ [Test(end_label, None)]
     @ c2
     @ [Pop; Goto(test_label, None); Label end_label; Push_unit])


let compile_arith e = 
  let (defs, c) = comp_arith e in result = c @ [Halt] @ defs
;;

let rec compile_bool e =
  let (defs, c) = comp_bool e in result = c @ [Halt] @ defs
;;
  

let rec compile_statement e = 
  let (defs, c) = comp_statement e in result = c @ [Halt] @ defs
;;

type value =
  | Unit
  | Int of int
  | Bool of bool
  | Closure of compiled_closure
;;

type env =
  (string * int) list
;;

let lookup env var =
  Int (List.Assoc.find_exn env ~equal:String.(=) var)
;;

exception Malformed_stack
;;

type stack =
  value list
;;

let one_bool = function
  | Bool a :: stack -> (a, stack)
  | _ ->  raise Malformed_stack
;;

let two_bools = function
  | Bool a :: Bool b :: stack -> (a, b, stack)
  | _ ->  raise Malformed_stack
;;

let one_int = function
  | Int a :: stack -> (a, stack)
  | _ -> raise Malformed_stack
;;

let two_ints = function
  | Int a :: Int b :: stack -> (a, b, stack)
  | _ ->  raise Malformed_stack
;;

let closure_and_int = function
  | Closure closure  :: Int int :: stack -> (closure, int, stack)
  | _ -> raise Malformed_stack
;;

let installed = ref (Array.of_list [Halt])
;;

let get_instruction pc = Array.get !installed cp
;;

let rec interp (pc, env, stack) =
  (match (get_instruction pc) with
  | Halt -> (pc, env, stack)

  | Lookup var -> interp (pc + 1, env, (lookup env var) :: stack)

  | Push_int int -> interp (pc + 1, env, Int int :: stack)

  | Plus ->
    let (a, b, new_stack) = two_ints stack in
    interp (pc + 1, env, Int (a+b) :: new_stack)

  | Times ->
    let (a, b, new_stack) = two_ints stack in
    interp (pc + 1, env, Int (a*b) :: new_stack)

  | Push_bool bool -> interp (pc + 1, env, Bool bool :: stack)

  | Not ->
    let (b, new_stack) = one_bool stack in
    interp (pc + 1, env, Bool (not b) :: new_stack)

  | And -> 
    let (a, b, new_stack) = two_bools stack in
    interp (cp + 1, env, Bool (a && b) :: new_stack)

  | Or ->
    let (a, b, new_stack) = two_bools stack in
    interp (pc + 1, env, Bool (a || b) :: new_stack)

  | Less_than ->
    let (a, b, new_stack) = two_ints stack in
    if a < b then interp (pc + 1, env, Bool true :: new_stack) else
		  interp (pc + 1, env, Bool false :: new_stack)

  | Greater_than ->
    let (a, b, new_stack) = two_ints stack in
    if a > b then interp (pc + 1, env, Bool true :: new_stack) else
		  interp (pc + 1, env, Bool false :: new_stack)

  | Equals ->
    let (a, b, new_stack) = two_ints stack in
    if a = b then interp (pc + 1, env, Bool true :: new_stack) else
		  interp (pc + 1, env, Bool false :: new_stack)

  | Assign var ->
    let (v, new_stack) = one_int stack in
    interp (pc + 1, (var, v) :: env, new_stack)

  | If (true_, false_) ->
    let (b, new_stack) =  one_bool stack in
    (match b with
    | true -> interp (pc + 1, env, new_stack) (true_ @ rest)
    | false -> interp (env, new_stack) (false_ @ rest))

  | While (cond, body) as loop ->
    let (b, new_stack) = one_bool stack in
    (match b with
    | true -> interp (env, new_stack) (body @ [loop] @ rest)
    | false -> interp (env, new_stack) rest))

  | Push_closure closure ->
    interp (pc + 1, env, (Closure closure) :: stack)

  | Call ->
    let ({arg; body}, int, stack) = closure_and_int stack in
    interp ((arg, First int) :: env, stack )

  | Pop_stack ->
    interp (pc + 1, env, List.tl_exn stack)

  | Pop_env ->
    interp (pc + 1, List.tl_exn env, stack)
;;

let eval statement =
  interp ([], []) (compile_statement statement)
;;
