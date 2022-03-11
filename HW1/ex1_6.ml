type expr =
  | NUM of int
  | PLUS of expr * expr
  | MINUS of expr * expr
  | MULT of expr * expr
  | DIVIDE of expr * expr
  | MAX of expr list

let rec eval (e: expr) : int =
  let rec calcMax (expr_list: expr list) : int =
    match expr_list with
    | expr_head::expr_tail -> (
      if expr_tail = [] then eval (expr_head)
      else if eval (expr_head) > (calcMax expr_tail) then eval (expr_head)
      else (calcMax expr_tail)
    )
    | _ -> 0
  in
  match e with
  | NUM tail_int -> tail_int
  | PLUS (expr1, expr2) -> (eval expr1) + (eval expr2)
  | MINUS (expr1, expr2) -> (eval expr1) - (eval expr2)
  | MULT (expr1, expr2) -> (eval expr1) * (eval expr2)
  | DIVIDE (expr1, expr2) -> (eval expr1) / (eval expr2)
  | MAX expr_list -> (
    match expr_list with
    | [] -> 0
    | _ -> calcMax expr_list
  )
