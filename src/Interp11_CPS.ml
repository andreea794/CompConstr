[@@@ocaml.warning "-27"]

(* 1: CPS *)
open Ast
;;

type value =
  int
;;

type env =
  string -> int
;;

let rec eval_arith (env, arith) return =
  match arith with
  | Var var -> return (env, env var)
  | Int int -> return (env, int)
  | Plus (a, b) ->
    eval_arith (env, a) (fun (a_env, a_int) ->
        eval_arith (a_env, b) (fun (b_env, b_int) ->
            return (b_env, a_int + b_int)))
  | Times (a, b) ->
    eval_arith (env, a) (fun (a_env, a_int) ->
        eval_arith (a_env, b) (fun (b_env, b_int) ->
            return (b_env, a_int * b_int)))
;;

let rec eval_bool (env, bool) return =
  match bool with
  | True -> return (env, true)
  | False -> return (env, false)
  | Not exp -> eval_bool (env, exp) (fun (exp_env, exp_val) -> return (exp_env, not exp_val))
  | And (first, second) ->
  	eval_bool (env, first) (fun (first_env, first_val) ->
		eval_bool (first_env, second) (fun (second_env, second_val) ->
			return (second_env, first_val && second_val)))
  | Or (first, second) ->
  	eval_bool (env, first) (fun (first_env, first_val) ->
		eval_bool (first_env, second) (fun (second_env, second_val) ->
			return (second_env, first_val || second_val)))
  | Bool_op (first, op, second) ->
    eval_arith (env, first) (fun (first_env, first_val) ->
		eval_arith (first_env, second) (fun (second_env, second_val) ->
		    (match op with
		    | Lt -> return (second_env, (first_val < second_val))
		    | Gt -> return (second_env, (first_val > second_val))
		    | Eq -> return (second_env, (first_val = second_val)))))
;;

let rec eval_statement (env, statement) return =
  match statement with
  | Skip -> return env
  | Assign (var, exp) ->
  	eval_arith (env, exp) (fun (exp_env, exp_val) ->
  		return (fun var' ->
  			eval_bool (exp_env, Bool_op(Var var, Eq, Var var')) (fun (new_env, bool_value) ->
  				if bool_value then exp_val else new_env var)))
  | Seq (first, second) ->
  	eval_statement (env, first) (fun first_env ->
  		eval_statement (first_env, second) (fun second_env ->
  			return second_env))
  | If (cond, true_, false_) ->
  	eval_bool (env, cond) (fun (cond_env, cond_val) ->
  		if cond_val then eval_statement (cond_env, true_) (fun true_env -> return true_env)
  					else eval_statement (cond_env, false_) (fun false_env -> return false_env))
  | While (cond, body) as loop ->
  	eval_bool (env, cond) (fun (cond_env, cond_val) ->
  		if cond_val then eval_statement (cond_env, body) (fun body_env ->
  							eval_statement (body_env, loop) (fun loop_env -> return loop_env))
  					else eval_statement (cond_env, loop) (fun loop_env -> return loop_env))
;;

let eval statement =
  eval_statement
    ((fun x -> failwith @@ "Not found: " ^ x), statement)
    (fun x -> x)
;;
