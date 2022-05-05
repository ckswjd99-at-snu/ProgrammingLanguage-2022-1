(*
 * SNU 4190.310 Programming Languages (Spring 2022)
 * Lambda Calculus
 * 
 * Kyuyeon Park
 * kypark@ropas.snu.ac.kr
*)
{
 open Parser
 exception Eof
 exception LexicalError
 let verbose1 s =  (* (print_string s; print_newline(); s) *) s
 let verbose2 s =  (* (print_string s; print_newline()) *) ()
 let comment_depth = ref 0
 let keyword_tbl = Hashtbl.create 31
 let _ = List.iter (fun (keyword, tok) -> Hashtbl.add keyword_tbl keyword tok)
        [("let", LET);
        ("in", IN);
        ]
} 

let blank = [' ' '\n' '\t' '\r']+
let id = ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '\'' '0'-'9' '_']*

rule start =
 parse blank { start lexbuf }
     | "(*" { comment_depth :=1;
              comment lexbuf;
              start lexbuf }
     | id { let id = verbose1 (Lexing.lexeme lexbuf)
            in try Hashtbl.find keyword_tbl id
               with _ -> ID id
            }
     | "\\" {verbose2 "+"; LAMBDA}
     | "=" {verbose2 "="; EQUAL}
     | "." {verbose2 "-"; DOT}
     | "(" { verbose2 "("; LP}
     | ")" { verbose2 ")"; RP}
     | eof { verbose2 "eof"; EOF}
     | _ { raise LexicalError}

and comment = parse
     "(*" {comment_depth := !comment_depth+1; comment lexbuf}
   | "*)" {comment_depth := !comment_depth-1;
           if !comment_depth > 0 then comment lexbuf }
   | eof {raise Eof}
   | _   {comment lexbuf}
