type crazy2 = 
  | NIL
  | ZERO of crazy2
  | ONE of crazy2
  | MONE of crazy2
;;

let rec crazy2val (c: crazy2) : int =
  match c with
  | NIL -> 0
  | ZERO following -> 0 + 2 * crazy2val following
  | ONE following -> 1 + 2 * crazy2val following
  | MONE following -> -1 + 2 * crazy2val following
;;


(* let test = crazy2val(
  ZERO(ONE(MONE NIL))
)
open Printf
let () = printf("%d\n") test *)