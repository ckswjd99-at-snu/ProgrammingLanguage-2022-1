(*
 * SNU 4190.310 Programming Languages 2022 Spring
 * Solution for HW 1-2 : sigma
*)

let rec sigma (a, b, f) =
  if a > b then 0 else (f a) + (sigma (a + 1, b, f))