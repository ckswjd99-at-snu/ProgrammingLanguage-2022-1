type ae =
  | CONST of int
  | VAR of string
  | POWER of string * int
  | TIMES of ae list
  | SUM of ae list

let rec reduceTimesList (aeList: ae list) : ae list =
  match aeList with
  | [] -> []
  | aeHead::aeTail -> (
    match aeHead with
    | CONST 0 -> [CONST 0]
    | CONST 1 -> reduceTimesList aeTail
    | TIMES subList -> (reduceTimesList subList)@(reduceTimesList aeTail)
    | _ -> (
      let reducedTail = reduceTimesList aeTail in
      match reducedTail with
      | [CONST 0] -> [CONST 0]
      | [CONST 1] -> [aeHead]
      | _ -> aeHead::reducedTail
    )
  )
;;

let testReduceTimes1 = reduceTimesList [CONST 1; CONST 2; CONST 3; CONST 0; CONST 4; CONST 5];;
let testReduceTimes2 = reduceTimesList [CONST 1; (TIMES [CONST 2; (TIMES [CONST 3; VAR "x"])]); CONST 5];;

let rec reduceSumList (aeList: ae list) : ae list =
  match aeList with
  | [] -> []
  | aeHead::aeTail -> (
    match aeHead with
    | CONST 0 -> reduceSumList aeTail
    | SUM subList -> (reduceSumList subList)@(reduceSumList aeTail)
    | _ -> aeHead::(reduceSumList aeTail)
  )
;;

let testReduceSum1 = reduceSumList [CONST 0; CONST 1; CONST 0; CONST 2; CONST 0; CONST 3];;
let testReduceSum2 = reduceSumList [CONST 0; SUM [CONST 1; VAR "x"; SUM[CONST 2; VAR "y"]]; TIMES [CONST 1; CONST 2]];;

let rec diff ((polynomial, coefficient): ae * string) : ae =
  let rawResult = (
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
      | [] -> CONST 0
      | aeHead::aeTail -> SUM [
        TIMES ((diff (aeHead, coefficient))::aeTail) ;
        TIMES (aeHead::(diff ((TIMES aeTail), coefficient)::[]))
      ]
    )
    | SUM aeList -> (
      match aeList with
      | [] -> CONST 0
      | aeHead::aeTail -> SUM [
        diff (aeHead, coefficient) ;
        diff ((SUM aeTail), coefficient)
      ]
    )
  ) in

  rawResult
;;

let test1 = SUM [
  TIMES [CONST 3; POWER ("x", 3)] ;
  TIMES [CONST 2; VAR "y"] ;
  CONST 5
];;
let result1 = diff (test1, "y");;