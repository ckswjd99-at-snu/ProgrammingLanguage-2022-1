open Ex2_1

let f1 = fun x -> 2 + x
let f2 = fun x -> fun y -> x * y

let _=
  let _ = Printf.printf("ex2-1: iter\n") in
  let print_bool x = print_endline (string_of_bool x) in
  print_bool (20 = (iter (10, (fun x->x+2))) 0);
  print_bool (100 = (iter (100, (fun x->x+1))) 0);
  print_bool (0 = (iter (10, (fun x->x))) 0);
  print_bool ((iter (10, f1)) 3 = 23);
  print_bool ((iter (2, f1)) 121 = f1 (f1 121));
  print_bool ((iter (3, f1)) 177 = f1 (f1 (f1 177)));
  print_bool ((iter (4, f2 1)) 44 = 44);
  print_bool (((iter (2, f2 7)) 4 = f2 7 (f2 7 4)));

  print_bool (iter (10, fun x -> x + 3) 100 = 130);
  print_bool (iter (0, fun x -> x + 3) 200 = 200);
  print_bool (iter (3, List.tl) [1;2;3;4;5;6] = [4;5;6]);
  print_bool (iter (4, (fun s -> s ^ s)) "a" = "aaaaaaaaaaaaaaaa");
  print_bool (iter (5, fun (x,y,z) -> (z, x, y)) (1,2,3) = (2, 3, 1));

  print_bool (iter (0, (fun x->x*3)) 3 = 3);
  print_bool (iter (0, (fun x->x mod 7)) 3 = 3);
  print_bool (iter (0, (fun x->x-3)) 3 = 3);
  print_bool (iter (10, (fun x -> x+1)) 50 = 60);
  print_bool (iter (14, (fun x -> x+10)) 10 = 150);
  print_bool (iter (5, fun x -> (-x)) 50 = -50);
  print_bool (iter (3, (fun x -> x*x)) 2 = 256);
  print_bool (iter (5, (fun x -> x*5)) 10 = 31250);
  print_bool (iter (1, (fun x->x mod 7)) 27 = 6);
  print_bool (iter (5, (fun x -> x/10)) 1234500 = 12);