(*
 * SNU 4190.310 Programming Languages (Spring 2022)
 * Lambda Calculus
 * 
 * Kyuyeon Park
 * kypark@ropas.snu.ac.kr
 *)

open Lambda
module Pp =
  struct
  	let rec pp exp =
		match exp with
		  Var s -> print_string s
		| Lam (s, e) -> print_string "\\"; print_string (s^"."); pp e; print_string ""
		| App (e1, e2) -> print_string "("; pp e1; print_string ") ("; pp e2; print_string ")"
  end
