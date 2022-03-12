type crazy2 = 
  | NIL
  | ZERO of crazy2
  | ONE of crazy2
  | MONE of crazy2

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
  
let crazy2add ((operand1, operand2): crazy2 * crazy2) : crazy2 =
  crazy2addFull (operand1, operand2, ZERO NIL)
