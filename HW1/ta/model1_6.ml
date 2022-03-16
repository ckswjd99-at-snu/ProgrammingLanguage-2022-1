(*
 * SNU 4190.310 Programming Languages 2022 Spring 
 * Solution for HW 1-6 : eval
 *)
type expr = NUM of int
  | PLUS of expr * expr
  | MINUS of expr * expr
  | MULT of expr * expr
  | DIVIDE of expr * expr
  | MAX of expr list

let rec eval expr =
  match expr with
  | NUM n -> n
  | PLUS (x,y) -> eval(x) + eval(y)
  | MINUS (x,y) -> eval(x) - eval(y)
  | MULT (x,y) -> eval(x) * eval(y)
  | DIVIDE (x,y) -> eval(x) / eval(y)
  | MAX list -> 
    match list with 
    | [] -> 0
    | h::[]-> eval(h)
    | h::t -> 
      (if eval h > eval(MAX t) then eval(h) else eval(MAX t))