open Ex1_6;;

let _=
  let _ = Printf.printf("ex1-6: eval number\n") in
  let print_bool x = print_endline (string_of_bool x) in

  print_bool(6 = eval(NUM 6))
;;