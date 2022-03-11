let rec sigma ((a, b, f) : int * int * (int -> int)) : int =
  if a > b then 0
  else (
    f b + sigma (a, b-1, f)
  )
;;


(* let square x = x * x;; let sigmaed = sigma (1, 4, square);; open Printf;; let () = printf("%d ") sigmaed;; *)
