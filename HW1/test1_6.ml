open Ex1_6

let _=
  let _ = Printf.printf("ex1-6: eval number\n") in
  let print_bool x = print_endline (string_of_bool x) in

  print_bool(6 = eval(NUM 6));
  print_bool(eval(PLUS (NUM 4, NUM 5)) = 9);
  print_bool(eval(MINUS (NUM 4, NUM 5)) = -1);
  print_bool(eval(MULT (NUM 4, NUM 5)) = 20);
  print_bool(eval(DIVIDE (NUM 5, NUM 3)) = 1);
  print_bool(eval(MAX [NUM 1; NUM 2; NUM 3; NUM 4; NUM 5]) = 5);
  print_bool(eval(MAX [NUM (-1); NUM (-2); NUM (-3); NUM (-4); NUM (-5)]) = -1);
