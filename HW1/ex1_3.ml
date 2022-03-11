type formula = 
  | TRUE
  | FALSE
  | NOT of formula
  | ANDALSO of formula * formula
  | ORELSE of formula * formula
  | IMPLY of formula * formula
  | LESS of expr * expr
and expr =
  | NUM of int
  | PLUS of expr * expr
  | MINUS of expr * expr

let rec eval_expr (e: expr) : int =
  match e with
  | NUM number -> number
  | PLUS (expr1, expr2) -> (eval_expr expr1) + (eval_expr expr2)
  | MINUS (expr1, expr2) -> (eval_expr expr1) - (eval_expr expr2)

let rec eval (f: formula) : bool =
  match f with
  | TRUE -> true
  | FALSE -> false
  | NOT following -> not (eval following)
  | ANDALSO (following1, following2) -> if (eval following1) then (eval following2) else false
  | ORELSE (following1, following2) -> if (eval following1) then true else (eval following2)
  | IMPLY (following1, following2) -> if not (eval following1) then true else (eval following2)
  | LESS (expr1, expr2) -> (eval_expr expr1) < (eval_expr expr2)
