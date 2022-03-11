open Ex1_2;;

let _=
  let _ = Printf.printf("ex1-2: sigma\n") in
  let print_bool x = print_endline (string_of_bool x) in
  print_bool(55 = sigma(1, 10, fun x -> x));
  print_bool(sigma(2,4,(fun x -> x+1)) = 12);
  print_bool(sigma(1,1000,(fun x -> 0)) = 0);
  print_bool(sigma(3,6,(fun x -> (x*x)+x)) = 104);
  print_bool(sigma(1,10,(fun x -> x mod 3)) = 10);
  print_bool(sigma(5,50,(fun x -> (x/3)+1)) = 452);
  print_bool(sigma(30,40,(fun x -> (x*x) mod 7)) = 26);
  print_bool(sigma(20,32,(fun y -> y / (y-15))) = 28);
  print_bool(sigma(3,10,(fun x -> x * (x/2))) = 178);
  print_bool(sigma((-2),0,(fun a -> a+a*a)) = 2);
  print_bool(sigma((-10),(-1),(fun a -> a+a+a)) = -165);
;;