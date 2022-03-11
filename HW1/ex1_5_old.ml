exception RemainWrong of string

type crazy2 = 
  | NIL
  | ZERO of crazy2
  | ONE of crazy2
  | MONE of crazy2

let rec crazy2val (c: crazy2) : int =
  match c with
  | NIL -> 0
  | ZERO following -> 0 + 2 * crazy2val following
  | ONE following -> 1 + 2 * crazy2val following
  | MONE following -> -1 + 2 * crazy2val following

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

let crazy2add ((a, b): crazy2 * crazy2) : crazy2 =
  let value = (crazy2val a) + (crazy2val b) in
  crazy2gen value
