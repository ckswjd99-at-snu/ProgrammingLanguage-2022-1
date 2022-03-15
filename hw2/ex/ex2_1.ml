exception VariableScope

type exp =
  | X
  | INT of int
  | REAL of float
  | ADD of exp * exp
  | SUB of exp * exp
  | MUL of exp * exp
  | DIV of exp * exp
  | SIGMA of exp * exp * exp
  | INTEGRAL of exp * exp * exp
;;

let rec calculate (e: exp) : float =
  let rec calcAnonymousExpr ((value, anonymousExpr): float * exp) : float =
    match anonymousExpr with
    | X -> value
    | INT following -> Float.of_int following
    | REAL following -> following
    | ADD (operand1, operand2) -> (calcAnonymousExpr (value, operand1)) +. (calcAnonymousExpr (value, operand2))
    | SUB (operand1, operand2) -> (calcAnonymousExpr (value, operand1)) -. (calcAnonymousExpr (value, operand2))
    | MUL (operand1, operand2) -> (calcAnonymousExpr (value, operand1)) *. (calcAnonymousExpr (value, operand2))
    | DIV (operand1, operand2) -> (calcAnonymousExpr (value, operand1)) /. (calcAnonymousExpr (value, operand2))
    | SIGMA _ -> calculate anonymousExpr
    | INTEGRAL _ -> calculate anonymousExpr
  in

  let rec sigma ((a, b, anonymousExpr) : int * int * exp) : float =
    if a > b then 0.
    else (
      let bAsFloat = Float.of_int b in
      let evalHead = calcAnonymousExpr (bAsFloat, anonymousExpr) in
      let evalTail = sigma (a, b-1, anonymousExpr) in
      evalHead +. evalTail
    )
  in

  let rec integral ((a, b, anonymousExpr) : float * float * exp) : float =
    if a > b then 0.
    else (
      let evalHead = calcAnonymousExpr (b, anonymousExpr) in
      let evalTail = integral (a, b -. 0.1, anonymousExpr) in
      evalHead +. evalTail
    )
  in

  match e with
  | X -> raise VariableScope
  | INT following -> Float.of_int following
  | REAL following -> following
  | ADD (following1, following2) -> (calculate following1) +. (calculate following2)
  | SUB (following1, following2) -> (calculate following1) -. (calculate following2)
  | MUL (following1, following2) -> (calculate following1) *. (calculate following2)
  | DIV (following1, following2) -> (calculate following1) /. (calculate following2)
  | SIGMA (operand1, operand2, anonymousExpr) -> (
    let indexFrom = Float.to_int (calculate operand1) in
    let indexTo = Float.to_int (calculate operand2) in
    sigma (indexFrom, indexTo, anonymousExpr)
  )
  | INTEGRAL (operand1, operand2, anonymousExpr) -> (
    let val1 = calculate operand1 in
    let val2 = calculate operand2 in
    integral (val1, val2, anonymousExpr)
  )


let test1 = SIGMA (INT 1, INT 10, SUB (MUL (X, X), INT 1));;
let test2 = INTEGRAL (REAL 1.0, REAL 10.0, SUB (MUL (X, X), INT 1));;
let () = Printf.printf("%f\n") (calculate test1);;
let () = Printf.printf("%f\n") (calculate test2);;