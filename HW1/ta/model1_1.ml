(*
 * SNU 4190.310 Programming Languages 2022 Spring
 * Solution for HW 1-1 : merge
*)

let rec merge (x, y) =
  match x, y with
  | [], _ -> y
  | _, [] -> x
  | hd1 :: tl1, hd2 :: tl2 ->
    if hd1 > hd2 then
      hd1 :: merge (tl1, y)
    else 
      hd2 :: merge (x, tl2)