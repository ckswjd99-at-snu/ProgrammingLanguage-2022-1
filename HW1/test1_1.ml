open Ex1_1;;

let _=
  let _ = Printf.printf("ex1-1: merge ordered list\n") in
  let print_bool x = print_endline (string_of_bool x) in
  print_bool([5;4;3;2;1] = merge([5;3;1], [4;2]))
;;
