(*
 * SNU 4190.310 Programming Languages (Spring 2022)
 * Lambda Calculus
 * 
 * Kyuyeon Park
 * kypark@ropas.snu.ac.kr
 *)

open Evaluate
open Pp 
open Lambda

let rec inline : lexp_let -> lexp = fun lexp_let ->
	match lexp_let with
	| LVar s -> Var s
	| LLam (s, e) -> Lam (s, inline e)
	| LApp (e1, e2) -> App(inline e1, inline e2)
	| Let (s, e1, e2) -> Evaluator.subs (s, inline e1) (inline e2)
 
let main () =
    let pp = ref false in
    let src = ref "" in
    let _ =
        Arg.parse
          [("-pp", Arg.Set pp, "display parse tree")]
          (fun x -> src := x)
          ("Usage: " ^ (Filename.basename Sys.argv.(0)) ^ " [-ptree] [file]")
    in
    let lexbuf = Lexing.from_channel (if !src = "" then stdin else open_in !src) in
    let pgm = inline (Parser.program Lexer.start lexbuf) in

	if !pp then
	(
		Lambda.pp pgm 0;
		print_newline()
	)
	else(
		print_string "=============\n";
		print_string "input program\n";
		print_string "=============\n";
		Pp.pp pgm;
		print_string "\n\n\n============\n";
		print_string "output program\n";
		print_string "=============\n";
		Pp.pp (Evaluator.reduce pgm);
		print_string "\n"
		)

let _ = main ()
