type value = int

type env = string -> (int, Ast0.closure) Either.t option

val eval_arith : env -> Ast0.arith_exp -> int

val eval_bool : env -> Ast0.bool_exp -> bool

val eval_statement : Ast0.statement -> env
