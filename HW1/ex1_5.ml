exception RemainWrong of string
exception WrongHalfAdderOutput of string

type crazy2 = 
  | NIL
  | ZERO of crazy2
  | ONE of crazy2
  | MONE of crazy2
;;

let rec crazy2addFull ((operand1, operand2, carry): crazy2 * crazy2 * crazy2) : crazy2 =
  match operand1, operand2 with
  | NIL, NIL -> carry

  | NIL, ZERO following2 -> (
    match carry with
    | NIL | ZERO _ -> operand2
    | ONE _ -> ONE following2
    | MONE _ -> MONE following2
  )
  | NIL, ONE following2 -> (
    match carry with
    | NIL | ZERO _ -> operand2
    | ONE _ -> ZERO (crazy2addFull (NIL, following2, ONE NIL))
    | MONE _ -> ZERO following2
  )
  | NIL, MONE following2 -> (
    match carry with
    | NIL | ZERO _ -> operand2
    | ONE _ -> ZERO following2
    | MONE _ -> ZERO (crazy2addFull (NIL, following2, MONE NIL))
  )
  
  | ZERO following1, NIL -> (
    match carry with
    | NIL | ZERO _ -> operand1
    | ONE _ -> ONE following1
    | MONE _ -> MONE following1
  )
  | ONE following1, NIL -> (
    match carry with
    | NIL | ZERO _ -> operand1
    | ONE _ -> ZERO (crazy2addFull (following1, NIL, ONE NIL))
    | MONE _ -> ZERO following1
  )
  | MONE following1, NIL -> (
    match carry with
    | NIL | ZERO _ -> operand1
    | ONE _ -> ZERO following1
    | MONE _ -> ZERO (crazy2addFull (following1, NIL, MONE NIL))
  )

  | ZERO following1, ZERO following2 -> (
    match carry with
    | NIL | ZERO _ -> ZERO (crazy2addFull (following1, following2, ZERO NIL))
    | ONE _ -> ONE (crazy2addFull (following1, following2, ZERO NIL))
    | MONE _ -> MONE (crazy2addFull (following1, following2, ZERO NIL))
  )
  | ZERO following1, ONE following2 -> (
    match carry with
    | NIL | ZERO _ -> ONE (crazy2addFull (following1, following2, ZERO NIL))
    | ONE _ -> ZERO (crazy2addFull (following1, following2, ONE NIL))
    | MONE _ -> ZERO (crazy2addFull (following1, following2, ZERO NIL))
  )
  | ZERO following1, MONE following2 -> (
    match carry with
    | NIL | ZERO _ -> MONE (crazy2addFull (following1, following2, ZERO NIL))
    | ONE _ -> ZERO (crazy2addFull (following1, following2, ZERO NIL))
    | MONE _ -> MONE (crazy2addFull (following1, following2, MONE NIL))
  )
  | ONE following1, ZERO following2 -> (
    match carry with
    | NIL | ZERO _ -> ONE (crazy2addFull (following1, following2, ZERO NIL))
    | ONE _ -> ZERO (crazy2addFull (following1, following2, ONE NIL))
    | MONE _ -> ZERO (crazy2addFull (following1, following2, ZERO NIL))
  )
  | ONE following1, ONE following2 -> (
    match carry with
    | NIL | ZERO _ -> ZERO (crazy2addFull (following1, following2, ONE NIL))
    | ONE _ -> ONE (crazy2addFull (following1, following2, ONE NIL))
    | MONE _ -> ONE (crazy2addFull (following1, following2, ZERO NIL))
  )
  | ONE following1, MONE following2 -> (
    match carry with
    | NIL | ZERO _ -> ZERO (crazy2addFull (following1, following2, ZERO NIL))
    | ONE _ -> ONE (crazy2addFull (following1, following2, ZERO NIL))
    | MONE _ -> MONE (crazy2addFull (following1, following2, ZERO NIL))
  )
  | MONE following1, ZERO following2 -> (
    match carry with
    | NIL | ZERO _ -> MONE (crazy2addFull (following1, following2, ZERO NIL))
    | ONE _ -> ZERO (crazy2addFull (following1, following2, ZERO NIL))
    | MONE _ -> ZERO (crazy2addFull (following1, following2, MONE NIL))
  )
  | MONE following1, ONE following2 -> (
    match carry with
    | NIL | ZERO _ -> ZERO (crazy2addFull (following1, following2, ZERO NIL))
    | ONE _ -> ONE (crazy2addFull (following1, following2, ZERO NIL))
    | MONE _ -> MONE (crazy2addFull (following1, following2, ZERO NIL))
  )
  | MONE following1, MONE following2 -> (
    match carry with
    | NIL | ZERO _ -> ZERO (crazy2addFull (following1, following2, MONE NIL))
    | ONE _ -> MONE (crazy2addFull (following1, following2, ZERO NIL))
    | MONE _ -> MONE (crazy2addFull (following1, following2, MONE NIL))
  )
;;
  
let crazy2add ((operand1, operand2): crazy2 * crazy2) : crazy2 =
  crazy2addFull (operand1, operand2, ZERO NIL)
;;

(* DEBUG AREA *)
(* let rec crazy2val (c: crazy2) : int =
  match c with
  | NIL -> 0
  | ZERO following -> 0 + 2 * crazy2val following
  | ONE following -> 1 + 2 * crazy2val following
  | MONE following -> -1 + 2 * crazy2val following
;;

let rec crazy2gen (value: int) : crazy2 =
  if value = 0 then NIL
  else (
    let share = value / 2 in
    let remain = value mod 2 in
    match remain with
    | -1 -> MONE (crazy2gen share)
    | 0 -> ZERO (crazy2gen share)
    | 1 -> ONE (crazy2gen share)
    | n -> raise (RemainWrong "remain wrong!")
  )
;;

let test1 = ZERO(ONE(ZERO(MONE(MONE(ONE(ZERO(ONE(ZERO(ZERO(ZERO(ZERO NIL)))))))))));;
let test1val = crazy2val(test1);;
let test2 = MONE(MONE(MONE(MONE(MONE(MONE NIL)))));;
let test2val = crazy2val(test2);;
let result = crazy2add(test1, test2);;
let resultAsVal = crazy2val(result);;
let () = Printf.printf("%d %d %d\n") test1val test2val resultAsVal;; *)
(* 
let rec c5 n =
  if n = 0 then NIL
  else ONE (MONE (c5 (n-1)))
;;
let rec c6 n =
  if n = 0 then NIL
  else MONE (ZERO (ONE (c6 (n-1))))

let rec crazy2val (c: crazy2) : int =
  match c with
  | NIL -> 0
  | ZERO following -> 0 + 2 * crazy2val following
  | ONE following -> 1 + 2 * crazy2val following
  | MONE following -> -1 + 2 * crazy2val following
;;

let operand1 = c5 4;; (* -85 *)
let operand2 = c6 3;; (* 219 *)
let operand3 = c6 3;; (* 219 *)
let operand4 = c5 5;; (* -341 *)
let result1 = crazy2add (operand1, operand2);; (* 134 *)
let result2 = crazy2add (operand3, operand4);; (* -122 *)
let result3 = crazy2add (result1, result2);; (* 12 *)
let result4 = crazy2val (result3);; *)
