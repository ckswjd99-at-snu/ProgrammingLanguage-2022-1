(*
 * SNU 4190.310 Programming Languages 2022 Spring
 * Solution for HW 1-4 : crazy2val
 *)

type crazy2 = NIL | ZERO of crazy2 | ONE of crazy2 | MONE of crazy2

(* The following line is equal to 
"let rec crazy2val x = match x with"
*)
let rec crazy2val = function
  | NIL -> 0
  | ZERO c -> 2 * (crazy2val c)
  | ONE c -> 1 + 2 * (crazy2val c)
  | MONE c -> -1 + 2 * (crazy2val c)