open Ex1_3;;

let f1 = TRUE
let f2 = FALSE
let f3 = NOT f1
let f4 = ANDALSO (NOT f2, ANDALSO (f3, f1))
let f5 = ORELSE (ORELSE (f3, f1), f4)
let f6 = IMPLY (f4, f5)
let f7 = IMPLY (f5, ORELSE (f4, FALSE))
let f8 = ORELSE (IMPLY (NOT f6, f2), ANDALSO (ORELSE (f3, NOT f4), NOT f7))
let f9 = LESS (NUM 1, NUM 2)
let fa = LESS (PLUS (NUM 1, NUM 2), MINUS (NUM 0, NUM 121))
let fb =
  LESS
    (MINUS
      (PLUS (NUM 5, MINUS (NUM 1, NUM 21)),
       MINUS (NUM 0, NUM 100)), NUM 2)
;;

let _=
  let _ = Printf.printf("ex1-3: boolean eval\n") in
  let print_bool x = print_endline (string_of_bool x) in
  print_bool(true = eval TRUE);
  print_bool(eval (IMPLY (NOT FALSE,NOT FALSE)) = true);
  print_bool(eval (IMPLY (NOT FALSE,ANDALSO (TRUE,FALSE))) = false);
  print_bool(eval (IMPLY (FALSE, FALSE)) = true);
  print_bool(eval (NOT (NOT (NOT (NOT (IMPLY (TRUE, TRUE)))))) = true);
  print_bool(eval (ANDALSO (ORELSE(TRUE, FALSE), NOT (IMPLY (TRUE, FALSE)))) = true);
  print_bool(eval (NOT (IMPLY (ANDALSO (FALSE, TRUE), ORELSE (FALSE, FALSE)))) = false);
  print_bool(eval (LESS (PLUS (MINUS (NUM 4, NUM 5), MINUS (NUM 1, NUM (-1))), PLUS (MINUS (NUM 3, NUM (-5)), PLUS (NUM 4, NUM 5)))) = true);
  print_bool(eval (ORELSE (LESS (MINUS (NUM 3, NUM 4), PLUS (NUM 3, NUM 4)), ANDALSO (NOT (TRUE), TRUE))) = true);
  print_bool(eval (ANDALSO (ANDALSO(TRUE, TRUE), ANDALSO (NOT (ANDALSO (TRUE, FALSE)), NOT (ORELSE (FALSE, FALSE))))) = true);
  print_bool(eval (IMPLY (IMPLY (LESS (NUM 3, MINUS (NUM 10, NUM 1)), ANDALSO (TRUE, TRUE)), IMPLY (ORELSE (TRUE, FALSE), NOT (TRUE)))) = false);

  print_bool (true = eval TRUE);
  print_bool (false = eval FALSE);
  print_bool (false = eval (NOT TRUE));
  print_bool (true = eval (NOT FALSE));
  print_bool (true = eval (ANDALSO (TRUE, TRUE)));
  print_bool (false = eval (ANDALSO (TRUE, FALSE)));
  print_bool (false = eval (ANDALSO (FALSE, TRUE)));
  print_bool (false = eval (ANDALSO (FALSE, FALSE)));
  print_bool (true = eval (ORELSE (TRUE, TRUE)));
  print_bool (true = eval (ORELSE (TRUE, FALSE)));
  print_bool (true = eval (ORELSE (FALSE, TRUE)));
  print_bool (false = eval (ORELSE (FALSE, FALSE)));
  print_bool (false = eval (IMPLY (TRUE, FALSE)));
  print_bool (true = eval (IMPLY (TRUE, TRUE)));
  print_bool (true = eval (IMPLY (FALSE, TRUE)));
  print_bool (true = eval (IMPLY (FALSE, FALSE)));
  print_bool (true = eval (LESS (NUM 3, NUM 5)));
  print_bool (false = eval (LESS (NUM 3, NUM 3)));
  print_bool (false = eval (LESS (NUM 3, NUM 1)));
  print_bool (false = eval (LESS (PLUS (NUM 3, NUM 4), MINUS (NUM 5, NUM 1))));
  print_bool (true = eval (LESS (PLUS (NUM 10, NUM 12), MINUS (NUM 10, NUM (-13)))));
  print_bool (eval f1 = true);
  print_bool (eval f2 = false);
  print_bool (eval f3 = false);
  print_bool (eval f4 = false);
  print_bool (eval f5 = true);
  print_bool (eval f6 = true);
  print_bool (eval f7 = false);
  print_bool (eval f8 = true);
  print_bool (eval f9 = true);
  print_bool (eval fa = false);
  print_bool (eval fb = false);

  print_bool (eval (IMPLY(FALSE, FALSE)) = true);
  print_bool (eval (ANDALSO (ORELSE(TRUE, FALSE), NOT (IMPLY(TRUE, FALSE)))) = true);
  print_bool ((eval (ANDALSO (TRUE, TRUE))) && (not (eval (ANDALSO (TRUE, FALSE)))) && (not (eval (ANDALSO (FALSE, TRUE)))) && (not (eval (ANDALSO (FALSE, FALSE)))) = true);
  print_bool ((eval (ORELSE (TRUE, TRUE))) && (eval (ORELSE (TRUE, FALSE))) && (eval (ORELSE (FALSE, TRUE))) && (not (eval (ORELSE (FALSE, FALSE)))) = true);
  print_bool ((eval (IMPLY (TRUE, TRUE))) && (not (eval (IMPLY (TRUE, FALSE)))) && (eval (IMPLY (FALSE, TRUE))) && (eval (IMPLY (FALSE, FALSE))) = true);
  print_bool (eval (LESS (NUM 3, NUM 5)) = true);
  print_bool (eval (LESS (PLUS (NUM 3, NUM 5), PLUS (NUM 1, NUM 2))) = false);
  print_bool (eval (LESS (MINUS (NUM 3, NUM 5), MINUS (NUM 1, NUM 2))) = true);
  print_bool (eval (ORELSE (LESS (PLUS (MINUS (NUM 3, NUM 2), NUM 9), NUM 10), FALSE)) = false);
  print_bool (eval (IMPLY(LESS (NUM 1, NUM 0), ANDALSO(TRUE, ORELSE(NOT TRUE, LESS(NUM 2, NUM 1))))) = true);
;;



