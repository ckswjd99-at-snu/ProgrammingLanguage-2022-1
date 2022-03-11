open Ex1_3;;

let _=
  let _ = Printf.printf("ex1-3: boolean eval\n") in
  let print_bool x = print_endline (string_of_bool x) in
  print_bool(true = eval TRUE)
;;