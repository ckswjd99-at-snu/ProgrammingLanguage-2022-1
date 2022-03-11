open Ex1_4;;
open Ex1_5_new;;

let _=
  let print_bool x = print_endline (string_of_bool x) in
  print_bool (-1  = (crazy2val (MONE NIL)));
  print_bool (1   = (crazy2val (ONE (ZERO (ZERO (ZERO NIL))))));
  print_bool (1   = (crazy2val (ONE NIL)));
  print_bool (9   = (crazy2val (MONE (MONE (ONE (ONE (ZERO NIL)))))));
  print_bool (-13 = (crazy2val (MONE (ZERO (ONE (ZERO (ONE (MONE NIL))))))))
;;
