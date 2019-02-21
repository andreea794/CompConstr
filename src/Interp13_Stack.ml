[@@@ocaml.warning "-27"]

(* 3: Stackify/instruction list *)
open Ast
;;

type value =
  | None
  | Int of int
  | Bool of bool
;;

type env =
  (string * int) list
;;

let lookup env var =
  Int (List.Assoc.find_exn env ~equal:String.(=) var)
;;

(* Elements of a list must be of the same type a.k.a. *)
(* the coproduct over the three continuation monoids *)
type cont =
  (* arith_cont *)
  | Lookup of string
  | Plus_with of int
  | Times_with of int
  | Compile_then_plus of arith_exp
  | Compile_then_times of arith_exp
  (* returns to eval_bool *)
  | Bool_op_with of bool_op * int
  | Compile_then_bool_op of bool_op * arith_exp
  (* returns to eval_statement *)
  | Update_env_then of string

  (* bool_cont *)
  | Not
  | And_with of bool
  | Or_with of bool
  | Compile_then_and of bool_exp
  | Compile_then_or of bool_exp
  (* returns to eval_statement *)
  | Branch of statement * statement
  | Loop of statement * statement

  (* statement_cont *)
  | Compile_then of statement
;;

let rec interp_arith (env, int) =
  match int with None | Bool _ -> assert false | Int int ->
  function
  | [] -> (env, Int int)
  | Lookup var :: rest -> interp_arith (env, lookup env var) rest
  | Plus_with int2 :: rest -> interp_arith (env, Int (int + int2)) rest
  | Times_with int2 :: rest -> interp_arith (env, Int (int * int2)) rest
  | Bool_op_with (op, int2) :: rest ->
    let bool = match op with
      | Lt -> int < int2
      | Gt -> int > int2
      | Eq -> int = int2 in
    interp_bool (env, Bool bool) rest
  | Compile_then_plus arith :: rest -> compile_arith (env, arith) (Plus_with int :: rest)
  | Compile_then_times arith :: rest -> compile_arith (env, arith) (Times_with int :: rest)
  | Compile_then_bool_op (op, arith) :: rest -> compile_arith (env, arith) (Bool_op_with (op, int) :: rest)
  | Update_env_then var :: rest -> interp_statement ((var, int) :: env) rest
  | _ -> assert false

and compile_arith (env, arith) rest =
  match arith with
  | Var var -> interp_arith (env, None) (Lookup var :: rest)
  | Int int -> interp_arith (env, Int int) rest
  | Plus (a, b) -> compile_arith (env, a) (Compile_then_plus b :: rest)
  | Times (a, b) -> compile_arith (env, a) (Compile_then_times b :: rest)

and interp_bool (env, bool) =
  match bool with None | Int _ -> assert false | Bool bool ->
  function
  | [] -> (env, Bool bool)
  | Not :: rest -> interp_bool (env, Bool (not bool)) rest
  | And_with bool2 :: rest -> interp_bool (env, Bool (bool && bool2)) rest
  | Or_with bool2 :: rest -> interp_bool (env, Bool (bool || bool2)) rest
  | Compile_then_and bool_exp :: rest -> compile_bool (env, bool_exp) (And_with bool :: rest)
  | Compile_then_or bool_exp :: rest -> compile_bool (env, bool_exp) (Or_with bool :: rest)
  | Branch (true_, false_) :: rest -> if bool then compile_statement (env, true_) rest else compile_statement (env, false_) rest
  | Loop (body, loop) :: rest -> if bool then compile_statement (env, body) (Compile_then loop :: rest) else compile_statement (env, loop) rest

and compile_bool (env, bool_exp) rest =
  match bool_exp with
  | True -> interp_bool (env, Bool true) rest
  | False -> interp_bool (env, Bool false) rest
  | Not exp -> compile_bool (env, exp) Not :: rest
  | And (first, second) -> compile_bool (env, first) Compile_then_and second :: rest
  | Or (first, second) -> compile_bool (env, first) Compile_then_or second :: rest
  | Bool_op (first, op, second) -> compile

and interp_statement env = function
  | [] -> (failwith "hole")
  | Compile_then second :: rest -> (failwith "hole")
  | _ -> assert false

and compile_statement (env, statement) rest =
  match statement with
  | Skip -> (failwith "hole")
  | Assign (var, arith_exp) -> (failwith "hole")
  | Seq (first, second) -> (failwith "hole")
  | If (cond, true_, false_) -> (failwith "hole")
  | While (cond, body) as loop -> (failwith "hole")
;;

let eval statement =
  compile_statement ([], statement) []
;;
