open Ex2_2

type env = (string * int) list

let rec find_env : env -> string -> int
= fun env x ->
	match env with
	| [] -> raise (Failure (x ^ "Not Found"))
	| (y, v)::tl -> if (y = x) then v else find_env tl x

let rec ae_eval = fun e env ->
	match e with
	| CONST n -> n
	| VAR x -> find_env env x 
	| POWER (x, n) -> 
		if n <= 0 then 1 else (find_env env x) * (ae_eval (POWER (x, n-1)) env) (* ??? *)
	| TIMES l ->
		begin 
			match l with
			| [] -> 0
			| [hd] -> ae_eval hd env
			| hd::tl -> (ae_eval hd env) * (ae_eval (TIMES tl) env)
		end
	| SUM l -> 
		begin
			match l with
			| [] -> 0
			| [hd] -> ae_eval hd env
			| hd::tl -> (ae_eval hd env) + (ae_eval (SUM tl) env)
		end

let _=
  let _ = Printf.printf("ex1-1: merge ordered list\n") in
  let print_bool x = print_endline (string_of_bool x) in
  print_bool ((ae_eval (diff (SUM [POWER ("x", 2); TIMES [CONST 2; VAR "x"]; CONST 1], "x")) [("x", 2)]) = 6);
  print_bool ((ae_eval (diff (SUM [POWER ("x", 2); POWER ("x", 2); CONST 1], "x")) [("x", 3)]) = 12);
  print_bool ((ae_eval (diff (SUM [POWER ("x", 2); POWER ("x", 2); CONST 1], "y")) [("x", 1)]) = 0);
  print_bool ((ae_eval (diff (TIMES [POWER ("x", 3); POWER ("y", 2)], "x")) [("x", 10); ("y", 5)]) = 7500);
  print_bool ((ae_eval (diff (SUM [TIMES [SUM [VAR "x"; VAR "y"]; TIMES [VAR "x"; VAR "y"]]; POWER ("x", 2)], "x")) [("x", 3); ("y", 4)]) = 46);
  print_bool ((ae_eval (diff (TIMES [TIMES [SUM [VAR "x"; VAR "y"]; VAR "x"]; VAR "x"], "x")) [("x", 2); ("y", 5)]) =  32);
  print_bool ((ae_eval (diff (TIMES [POWER ("x",2); VAR "y"], "x")) [("x", 3); ("y", 4)]) = 24);
  print_bool ((ae_eval (diff (TIMES [CONST 2; SUM [VAR "x"; VAR "y"]; POWER ("x", 3)], "x")) [("x", 2); ("y", 1)]) = 88);
  print_bool ((ae_eval (diff (TIMES [SUM [VAR "x"; VAR "y"; VAR "z"]; POWER ("x", 2); SUM[TIMES [CONST 3; VAR "x"]; VAR "z"]], "x")) [("x", 2); ("y", 1); ("z", 1)]) = 188);
  print_bool ((ae_eval (diff (TIMES [SUM [VAR "x"; VAR "y"; VAR "z"]; POWER ("x", 2); SUM[TIMES [CONST 3; VAR "x"]; VAR "z"]], "y")) [("x", 1); ("y", 1); ("z", 1)]) = 4);
