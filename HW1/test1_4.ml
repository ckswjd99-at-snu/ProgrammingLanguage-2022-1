open Ex1_4;;

let c1 = ONE NIL
let c2 = ONE (ZERO (ONE NIL))
let c3 = ONE (MONE NIL)
let c4 = ONE (MONE (ZERO (MONE NIL)))
let rec c5 n =
  if n = 0 then NIL
  else ONE (MONE (c5 (n-1)))
let rec c6 n =
  if n = 0 then NIL
  else MONE (ZERO (ONE (c6 (n-1))))

let _=
  let _ = Printf.printf("ex1-4: crazy2val\n") in
  let print_bool x = print_endline (string_of_bool x) in
  print_bool (-1  = (crazy2val (MONE NIL)));
  print_bool (1   = (crazy2val (ONE (ZERO (ZERO (ZERO NIL))))));
  print_bool (1   = (crazy2val (ONE NIL)));
  print_bool (9   = (crazy2val (MONE (MONE (ONE (ONE (ZERO NIL)))))));
  print_bool (-13 = (crazy2val (MONE (ZERO (ONE (ZERO (ONE (MONE NIL))))))));
  print_bool (crazy2val c1 = 1);
  print_bool (crazy2val c2 = 5);
  print_bool (crazy2val c3 = -1);
  print_bool (crazy2val c4 = -9);
  print_bool (crazy2val (c5 1) = -1);
  print_bool (crazy2val (c5 2) = -5);
  print_bool (crazy2val (c5 3) = -21);
  print_bool (crazy2val (c5 4) = -85);
  print_bool (crazy2val (c5 5) = -341);
  print_bool (crazy2val (c6 1) = 3);
  print_bool (crazy2val (c6 2) = 27);
  print_bool (crazy2val (c6 3) = 219);
;;

(* let _ = Printf.printf("%d\n") (crazy2val (MONE (ONE (ONE (ZERO (MONE (ONE (ONE (ZERO (ONE (ZERO (MONE NIL)))))))))))) *)
