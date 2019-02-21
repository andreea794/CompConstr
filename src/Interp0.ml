[@@@ocaml.warning "-27"]

open Ast0
;;

type env =
  string -> value
;;

let rec eval_arith env = function
  | Var var -> (env var)
  | Arith_Int int -> Int int
  | Arith_Fun f -> Fun f
  | Plus (a, b) ->
  let x = eval_arith env a in
  let y = eval_arith env b in
  (match (x, y) with
  | (Int m, Int n) -> Int (m + n)
  | (_, _) -> assert false)
  | Times (a, b) ->
  let x = eval_arith env a in
  let y = eval_arith env b in
  (match (x, y) with
  | (Int m, Int n) -> Int (m * n)
  | (_, _) -> assert false)
;;

let rec eval_bool env = function
  | True -> true
  | False -> false
  | Not exp -> if (eval_bool env exp) then false else true
  | And (first, second) -> if (eval_bool env first) && (eval_bool env second) then true else false
  | Or (first, second) -> if (eval_bool env first) || (eval_bool env second) then true else false
  | Bool_op (first, op, second) -> 
  let x = eval_arith env first in
  let y = eval_arith env second in
  (match  (x, y) with
  | (Int m, Int n) ->
    (match op with
    | Lt -> m < n
    | Gt -> m > n
    | Eq -> m = n)
  | (_, _) -> assert false)
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
  | LetFun (f, (x, body), exp) ->
    let new_function = (fun v : int-> let new_env = eval_statement env (Assign(x, Arith_Int v)) in 
    (match (eval_arith new_env body) with
    | Int val -> val
    | Fun f -> assert false)) in
    let new_new_env = eval_statement env (Assign(f, Arith_Fun new_function)) in
    eval_statement new_new_env exp
  (* | LetFunRec (f, (x, body), exp) ->
    let rec  *)


;;

let eval_statement =
  eval_statement (fun x -> failwith @@ "Not found: " ^ x)
;;
