exception RemainWrong of string
exception WrongHalfAdderOutput of string

type crazy2 = 
  | NIL
  | ZERO of crazy2
  | ONE of crazy2
  | MONE of crazy2
;;

let crazy2addHalf ((operand1, operand2): crazy2 * crazy2) : crazy2 * crazy2 =
  match operand1 with
  | NIL -> (
    match operand2 with
    | NIL -> (ZERO NIL, ZERO NIL)
    | ZERO _ -> (ZERO NIL, ZERO NIL)
    | ONE _ -> (ONE NIL, ZERO NIL)
    | MONE _ -> (MONE NIL, ZERO NIL)
  )
  | ZERO _ -> (
    match operand2 with
    | NIL -> (ZERO NIL, ZERO NIL)
    | ZERO _ -> (ZERO NIL, ZERO NIL)
    | ONE _ -> (ONE NIL, ZERO NIL)
    | MONE _ -> (MONE NIL, ZERO NIL)
  )
  | ONE _ -> (
    match operand2 with
    | NIL -> (ONE NIL, ZERO NIL)
    | ZERO _ -> (ONE NIL, ZERO NIL)
    | ONE _ -> (ZERO NIL, ONE NIL)
    | MONE _ -> (ZERO NIL, ZERO NIL)
  )
  | MONE _ -> (
    match operand2 with
    | NIL -> (MONE NIL, ZERO NIL)
    | ZERO _ -> (MONE NIL, ZERO NIL)
    | ONE _ -> (ZERO NIL, ZERO NIL)
    | MONE _ -> (ZERO NIL, MONE NIL)
  )

let rec crazy2addFull ((operand1, operand2, carry): crazy2 * crazy2 * crazy2) : crazy2 =
  let (sum1, carry1) : crazy2 * crazy2 = crazy2addHalf (operand1, operand2) in
  let (sum, carry2) : crazy2 * crazy2 = crazy2addHalf (sum1, carry) in
  let (carry, _) : crazy2 * crazy2 = crazy2addHalf (carry1, carry2) in
  
  match operand1 with
    | NIL -> (
      match operand2 with
      | NIL -> NIL
      | _ -> operand2
    )
    | ZERO following1
    | ONE following1
    | MONE following1 -> (
      match operand2 with
      | NIL -> operand1
      | ZERO following2
      | ONE following2
      | MONE following2 -> (
        match sum with
        | NIL -> raise (WrongHalfAdderOutput "half adder output cannot be NIL!")
        | ZERO _ -> ZERO (crazy2addFull (following1, following2, carry))
        | ONE _ -> ONE (crazy2addFull (following1, following2, carry))
        | MONE _ -> MONE (crazy2addFull (following1, following2, carry))
      )      
    )
  
let crazy2add ((operand1, operand2): crazy2 * crazy2) : crazy2 =
  crazy2addFull (operand1, operand2, ZERO NIL)


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
