(*
 * SNU 4190.310 Programming Languages (Spring 2022)
 *
 * Lambda Calculus
 *)

type lexp = Var of string
		  | Lam of string * lexp
		  | App of lexp * lexp

type lexp_let = LVar of string
		  | LLam of string * lexp_let
		  | LApp of lexp_let * lexp_let
			| Let of string * lexp_let * lexp_let

let rec pp lexp i =
	match lexp with
	  Var s -> print_string (indent i); print_string "Var "; print_string s
	| Lam (s, e) -> print_string (indent i); print_string ("Lam ("^s^",\n"); pp e (i + 1);print_string ("\n"^(indent i)^")")
	| App (e1, e2) -> print_string (indent i); print_string ("App (\n"); pp e1 (i+1); print_string ", \n"; pp e2 (i+1); print_string ("\n"^(indent i)^")")
and indent i =
	if i = 0 then ""
	else if i = 1 then "  "
	else "  "^(indent (i-1))


