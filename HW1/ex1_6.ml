exception EmptyExprList of string;;

type expr =
  | NUM of int
  | PLUS of expr * expr
  | MINUS of expr * expr
  | MULT of expr * expr
  | DIVIDE of expr * expr
  | MAX of expr list
;;

let rec eval (e: expr) : int =
  let rec calcMax (expr_list: expr list) : int =
    match expr_list with
    | expr_head::expr_tail -> (
      if expr_tail = [] then eval (expr_head)
      else if eval (expr_head) > (calcMax expr_tail) then eval (expr_head)
      else (calcMax expr_tail)
    )
    | _ -> raise (EmptyExprList "expr list is empty!")
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
;;


(* let test = PLUS(
  MINUS(
    DIVIDE(NUM 16, NUM 3),
    NUM 1
  ),
  MULT(
    NUM 5,
    MAX [NUM 4; NUM 1; NUM 2; NUM 3; NUM 5]
  )
);;
let result = eval test;;
open Printf
let () = printf("%d\n") result;; *)