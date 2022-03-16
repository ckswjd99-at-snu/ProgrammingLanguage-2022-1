(*
 * SNU 4190.310 Programming Languages 2022 Spring
 * Solution for HW 1-3 : formula
*)

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
 
(* evaluate expr to integer *)
let rec eval_expr e =
  match e with 
  | NUM i -> i
  | PLUS (e1, e2) -> (eval_expr e1) + (eval_expr e2)
  | MINUS (e1, e2) -> (eval_expr e1) - (eval_expr e2)

(* evaluate formula to bool *)
let rec eval f =
  match f with
  | TRUE -> true
  | FALSE -> false
  | NOT f -> not (eval f)
  | ANDALSO (f1, f2) -> (eval f1) && (eval f2)
  | ORELSE (f1, f2) -> (eval f1) || (eval f2)
  | IMPLY (f1, f2) -> (not (eval f1)) || (eval f2)
  | LESS (e1, e2) -> (eval_expr e1) < (eval_expr e2)