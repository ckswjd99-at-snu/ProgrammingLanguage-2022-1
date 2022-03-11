open Ex1_2

let foo = fun n -> n * 2
let bar = fun n -> n * n

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
  print_bool (385 = sigma (1, 10, fun x -> x*x));
  print_bool (0 = sigma (3, 1, fun x -> x*x));
  print_bool (27 = sigma (3, 3, fun x -> x*x*x));
  print_bool (385 = sigma (-10, -1, fun x -> x*x));
  print_bool (sigma (3, 1, foo) = 0);
  print_bool (sigma (4, 2, bar) = 0);
  print_bool (sigma (8, 8, foo) = 16);
  print_bool (sigma (3, 4, bar) = 25);
  print_bool (sigma (1, 10, foo) = 110);
  print_bool (sigma (1, 10, bar) = 5 * 7 * 11);
  print_bool (sigma (5, 10, foo) = 90);
  print_bool (sigma (1, 100, foo) = 10100);
  print_bool (sigma (10, 10, fun x -> x) = 10);
  print_bool (sigma (11, 10, fun x -> x) = 0);
  print_bool (sigma (10, 5, fun x -> x) = 0);
  print_bool (sigma (1, 10, fun x -> if x mod 2 = 0 then 1 else 0 ) = 5);
  print_bool (sigma (1, 10, fun x -> x * x) = 385);
