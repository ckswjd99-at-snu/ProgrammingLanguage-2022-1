exception InvalidArgument

type ae =
  | CONST of int
  | VAR of string
  | POWER of string * int
  | TIMES of ae list
  | SUM of ae list

let rec diff ((polynomial, coefficient): ae * string) : ae =
  match polynomial with
  | CONST following -> CONST 0
  | VAR following -> (
    if following = coefficient 
    then CONST 1
    else CONST 0
  )
  | POWER (variable, exponent) -> (
    if variable = coefficient
    then TIMES [(CONST exponent); (POWER (variable, exponent - 1))]
    else CONST 0
  )
  | TIMES aeList -> (
    match aeList with
    | [] -> raise InvalidArgument
    | aeHead::aeTail -> SUM [
      TIMES ((diff (aeHead, coefficient))::aeTail) ;
      TIMES (aeHead::(diff ((TIMES aeTail), coefficient)::[]))
    ]
  )
  | SUM aeList -> (
    match aeList with
    | [] -> raise InvalidArgument
    | aeHead::aeTail -> SUM [
      diff (aeHead, coefficient) ;
      diff ((SUM aeTail), coefficient)
    ]
  )
