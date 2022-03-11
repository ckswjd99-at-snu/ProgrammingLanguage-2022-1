open Ex1_2;;

let _=
  let _ = Printf.printf("ex1-2: sigma\n") in
  let print_bool x = print_endline (string_of_bool x) in
  print_bool(55 = sigma(1, 10, fun x -> x))
;;