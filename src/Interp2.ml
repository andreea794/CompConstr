[@@@ocaml.warning "-27"]

open Ast
;;

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
;;

let rec compile_arith = function
  | Var var -> [Lookup var]
  | Int int -> [Push_int int]
  | Plus (a, b) -> compile_arith a @ compile_arith b @ [Plus]
  | Times (a, b) -> compile_arith a @ compile_arith b @ [Times]
;;

let rec compile_bool = function
  | True -> [Push_bool true]
  | False -> [Push_bool false]
  | Not exp -> (compile_bool exp) @ [Not]
  | And (first, second) -> (compile_bool first) @ (compile_bool second) @ [And]
  | Or (first, second) -> (compile_bool first) @ (compile_bool second) @ [Or]
  | Bool_op (first, op, second) ->
    match op with
    | Lt -> (compile_arith first) @ (compile_arith second) @ [Less_than]
    | Gt -> (compile_arith first) @ (compile_arith second) @ [Greater_than]
    | Eq -> (compile_arith first) @ (compile_arith second) @ [Equals]

let rec compile_statement = function
  | Skip ->  []
  | Assign (var, exp) -> [Assign var] @ (compile_arith exp)
  | Seq (first, second) -> compile_statement first @ compile_statement second
  | If (cond, true_, false_) -> (compile_bool cond) @ [If (compile_statement true_, compile_statement false_)]
  | While (cond, body) -> let result = compile_bool cond in
    result @ [While (result, compile_statement body)]
;;

type value =
  | Int of int
  | Bool of bool
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

let rec interp (env, stack) = function

  | [] -> (env, stack)

  | Lookup var :: rest -> interp (env, (lookup env var) :: stack) rest

  | Push_int int :: rest -> interp (env, Int int :: stack) rest

  | Plus :: rest ->
    let (a, b, new_stack) = two_ints stack in
    interp (env, Int (a+b) :: new_stack) rest

  | Times :: rest ->
    let (a, b, new_stack) = two_ints stack in
    interp (env, Int (a*b) :: new_stack) rest

  | Push_bool bool :: rest -> interp (env, Bool bool :: stack) rest

  | Not :: rest ->
    let (b, new_stack) = one_bool stack in
    interp (env, Bool (not b) :: new_stack) rest

  | And :: rest -> 
    let (a, b, new_stack) = two_bools stack in
    interp (env, Bool (a && b) :: new_stack) rest

  | Or :: rest ->
    let (a, b, new_stack) = two_bools stack in
    interp (env, Bool (a || b) :: new_stack) rest

  | Less_than :: rest ->
    let (a, b, new_stack) = two_ints stack in
    if a < b then interp (env, Bool true :: new_stack) rest else
		  interp (env, Bool false :: new_stack) rest

  | Greater_than :: rest ->
    let (a, b, new_stack) = two_ints stack in
    if a > b then interp (env, Bool true :: new_stack) rest else
		  interp (env, Bool false :: new_stack) rest

  | Equals :: rest ->
    let (a, b, new_stack) = two_ints stack in
    if a = b then interp (env, Bool true :: new_stack) rest else
		  interp (env, Bool false :: new_stack) rest

  | Assign var :: rest ->
    let (v, new_stack) = one_int stack in
    interp ((var, v) :: env, new_stack) rest

  | If (true_, false_) :: rest ->
    let (b, new_stack) =  one_bool stack in
    (match b with
    | true -> interp (env, new_stack) (true_ @ rest)
    | false -> interp (env, new_stack) (false_ @ rest))

  | While (cond, body) as loop :: rest ->
    let (b, new_stack) = one_bool stack in
    (match b with
    | true -> interp (env, new_stack) (body @ [loop] @ rest)
    | false -> interp (env, new_stack) rest)

;;

let eval statement =
  interp ([], []) (compile_statement statement)
;;
