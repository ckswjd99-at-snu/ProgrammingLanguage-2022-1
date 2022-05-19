type token =
  | IF
  | THEN
  | ELSE
  | FN
  | RARROW
  | DOT
  | PLUS
  | MINUS
  | LP
  | RP
  | REC
  | COMMA
  | EOF
  | NUM of (int)
  | ID of (string)

val program :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> M0.nexp
