open Ex1_1

let l0 = []
let l1 = [5; 3; 1]
let l2 = [4; 2; 0]
let l3 = [10; 7; 6]
let l4 = [11; 9; 8]

let _=
  let _ = Printf.printf("ex1-1: merge ordered list\n") in
  let print_bool x = print_endline (string_of_bool x) in
  print_bool([5;4;3;2;1] = merge([5;3;1], [4;2]));
  print_bool ([10; 10; 4; 3; 2; 1] = (merge ([10; 4; 2], [10; 3; 1])));
  print_bool ([10; 3; 2; -2; -3; -10] = (merge ([3; -2; -10], [10; 2; -3])));
  print_bool (merge (l0, l0) = l0);
  print_bool (merge (l1, l0) = l1);
  print_bool (merge (l0, l2) = l2);
  print_bool (merge (l1, l2) = [5; 4; 3; 2; 1; 0]);
  print_bool (merge (l2, l1) = [5; 4; 3; 2; 1; 0]);
  print_bool (merge (l3, l4) = [11; 10; 9; 8; 7; 6]);
  print_bool (merge (l4, l3) = [11; 10; 9; 8; 7; 6]);
  print_bool (merge (l1, l4) = [11; 9; 8; 5; 3; 1]);
  print_bool (merge (merge (l1, l2), l4) = [11; 9; 8; 5; 4; 3; 2; 1; 0]);
  print_bool (merge (merge (l1, l2), merge (l4, l3)) = [11; 10; 9; 8; 7; 6; 5; 4; 3; 2; 1; 0]);
  print_bool (merge ([], [5;4;3;2;1]) = [5;4;3;2;1]);
  print_bool (merge ([10;8;6;4;2], []) = [10;8;6;4;2]);
  print_bool (merge ([3;2;1], [6;5;4]) = [6;5;4;3;2;1]);
  print_bool (merge ([5;3;1], [4;2]) = [5;4;3;2;1]);
  print_bool (merge ([10;2;1], [10;4;3]) = [10;10;4;3;2;1]);
