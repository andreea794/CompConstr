[@@@ocaml.warning "-27"]

open Ast
;;

type value =
  int
;;

type env =
  string -> int
;;

let rec eval_arith env = function
  | Var var -> (env var)
  | Int int -> int
  | Plus (a, b) -> (eval_arith env a) + (eval_arith env b)
  | Times (a, b) -> (eval_arith env a) * (eval_arith env b)
;;

let rec eval_bool env = function
  | True -> true
  | False -> false
  | Not exp -> if (eval_bool env exp) then false else true
  | And (first, second) -> if (eval_bool env first) && (eval_bool env second) then true else false
  | Or (first, second) -> if (eval_bool env first) || (eval_bool env second) then true else false
  | Bool_op (first, op, second) -> match op with
	| Lt -> if (eval_arith env first) < (eval_arith env second) then true else false
	| Gt -> if (eval_arith env first) > (eval_arith env second) then true else false
	| Eq -> if (eval_arith env first) = (eval_arith env second) then true else false
;;

let rec eval_statement env = function
  | Skip -> env
  | Assign (var, exp) ->
    let result = eval_arith env exp in
    let new_env = (fun var' -> if (eval_bool env (Bool_op(Var var, Eq, Var var'))) then result else env var') in
    new_env
  | Seq (first, second) ->
    let new_env = eval_statement env first in eval_statement new_env second
  | If (cond, true_, false_) ->
    let result = eval_bool env cond in
    (match result with
    | true -> eval_statement env true_
    | false -> eval_statement env false_)
  | While (cond, body) as loop ->
    let result = eval_bool env cond in
    (match result with
    | true -> eval_statement env (Seq (body, loop))
    | false -> env)
;;

let eval_statement =
  eval_statement (fun x -> failwith @@ "Not found: " ^ x)
;;
