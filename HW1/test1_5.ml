open Ex1_5;;

let c0 = ZERO NIL;;
let c1 = ONE NIL;;
let c2 = ONE (ZERO (ONE NIL));;
let c3 = ONE (MONE NIL);;
let c4 = ONE (MONE (ZERO (MONE NIL)));;
let rec c5 n =
  if n = 0 then NIL
  else ONE (MONE (c5 (n-1)))
;;
let rec c6 n =
  if n = 0 then NIL
  else MONE (ZERO (ONE (c6 (n-1))))
;;
let c7 = MONE (ZERO (ZERO (ZERO NIL)));;
let c8 =
  ZERO (ONE (ONE (ZERO (MONE (ONE (ONE (ZERO (ONE (ZERO (MONE NIL))))))))))
;;
let zero = ZERO NIL;;
let one = MONE (ONE (ZERO NIL));;
let four = ZERO (ZERO (MONE (ONE NIL)));;
let m_five = ONE (MONE (ONE (MONE NIL)));;
let m_one = (ONE(ONE(ONE(ONE(ONE(ONE(ONE(ONE(ONE(ONE(ONE(ONE(ONE(ONE(ONE(ONE(ONE(ONE(ONE(ONE(ONE(ONE(ONE(ONE(ONE(ONE(ONE(ONE(ONE(MONE NIL))))))))))))))))))))))))))))));;


let rec crazy2val: crazy2 -> int = fun c ->
  match c with
  | NIL -> 0
  | ZERO following -> 0 + 2 * crazy2val following
  | ONE following -> 1 + 2 * crazy2val following
  | MONE following -> -1 + 2 * crazy2val following
;;

let _=
  let _ = Printf.printf("ex1-5: crazy2add\n") in
  let print_bool x = print_endline (string_of_bool x) in

  (* 1 *)
  print_bool (0 = (crazy2val (crazy2add (ZERO NIL, ZERO NIL))));
  print_bool (0 = (crazy2val (crazy2add (MONE NIL, ONE NIL))));
  print_bool (1 = (crazy2val (crazy2add (ZERO NIL, ONE NIL))));
  print_bool (4 = (crazy2val (crazy2add (ONE (ONE NIL), ONE NIL))));
  print_bool (-683 = (crazy2val (crazy2add (MONE (ZERO (ZERO (ZERO NIL))), (ZERO (ONE (ONE (ZERO (MONE (ONE (ONE (ZERO (ONE (ZERO (MONE NIL)))))))))))))));
  print_bool(crazy2val (crazy2add (c0, c0)) = 0);
  print_bool(crazy2val (crazy2add (c0, c1)) = 1);
  print_bool(crazy2val (crazy2add (c1, c0)) = 1);
  print_bool(crazy2val (crazy2add (c0, c2)) = 5);
  print_bool(crazy2val (crazy2add (c2, c0)) = 5);
  print_bool(crazy2val (crazy2add (c0, c3)) = -1);
  print_bool(crazy2val (crazy2add (c3, c0)) = -1);
  print_bool(crazy2val (crazy2add (c0, c4)) = -9);
  print_bool(crazy2val (crazy2add (c4, c0)) = -9);

  (* 15 *)
  print_bool(crazy2val (crazy2add (c1, c1)) = 2);
  print_bool(crazy2val (crazy2add (c1, c2)) = 6);
  print_bool(crazy2val (crazy2add (c2, c1)) = 6);
  print_bool(crazy2val (crazy2add (c1, c3)) = 0);
  print_bool(crazy2val (crazy2add (c3, c1)) = 0);
  print_bool(crazy2val (crazy2add (c1, c4)) = -8);
  print_bool(crazy2val (crazy2add (c4, c1)) = -8);

  (* 22 *)
  print_bool(crazy2val (crazy2add (c2, c2)) = 10);
  print_bool(crazy2val (crazy2add (c2, c3)) = 4);
  print_bool(crazy2val (crazy2add (c3, c2)) = 4);
  print_bool(crazy2val (crazy2add (c2, c4)) = -4);
  print_bool(crazy2val (crazy2add (c4, c2)) = -4);

  (* 27 *)
  print_bool(crazy2val (crazy2add (c3, c3)) = -2);
  print_bool(crazy2val (crazy2add (c3, c4)) = -10);
  print_bool(crazy2val (crazy2add (c4, c3)) = -10);

  (* 30 *)
  print_bool(crazy2val (crazy2add (c4, c4)) = -18);

  (* 31 *)
  print_bool(crazy2val (crazy2add (c5 1, c5 1)) = -2);
  print_bool(crazy2val (crazy2add (c5 2, c5 1)) = -6);
  print_bool(crazy2val (crazy2add (c5 3, c5 2)) = -26);
  print_bool(crazy2val (crazy2add (c5 2, c5 5)) = -346);

  (* 35 *)
  print_bool(crazy2val (crazy2add (c6 1, c5 1)) = 2);
  print_bool(crazy2val (crazy2add (c5 4, c6 2)) = -58);
  print_bool(crazy2val (crazy2add (c6 3, c5 4)) = 134);
  print_bool(crazy2val (crazy2add (crazy2add (c5 4, c6 3), (crazy2add (c6 3, c5 5)))) = 12);
  print_bool(crazy2val (crazy2add (c7, c8)) = (-683));

  (* 40 *)
  print_bool (  0 = crazy2val (crazy2add (zero, zero)));
  print_bool (  1 = crazy2val (crazy2add (zero, one)));
  print_bool (  4 = crazy2val (crazy2add (zero, four)));
  print_bool ( -5 = crazy2val (crazy2add (zero, m_five)));
  print_bool ( -1 = crazy2val (crazy2add (zero, m_one)));
  print_bool (  1 = crazy2val (crazy2add (one, zero)));
  print_bool (  2 = crazy2val (crazy2add (one, one)));
  print_bool (  5 = crazy2val (crazy2add (one, four)));
  print_bool ( -4 = crazy2val (crazy2add (one, m_five)));
  print_bool (  0 = crazy2val (crazy2add (one, m_one)));
  print_bool (  4 = crazy2val (crazy2add (four, zero)));
  print_bool (  5 = crazy2val (crazy2add (four, one)));
  print_bool (  8 = crazy2val (crazy2add (four, four)));
  print_bool ( -1 = crazy2val (crazy2add (four, m_five)));
  print_bool (  3 = crazy2val (crazy2add (four, m_one)));
  print_bool ( -5 = crazy2val (crazy2add (m_five, zero)));
  print_bool ( -4 = crazy2val (crazy2add (m_five, one)));
  print_bool ( -1 = crazy2val (crazy2add (m_five, four)));
  print_bool (-10 = crazy2val (crazy2add (m_five, m_five)));
  print_bool ( -6 = crazy2val (crazy2add (m_five, m_one)));
  print_bool ( -1 = crazy2val (crazy2add (m_one, zero)));
  print_bool (  0 = crazy2val (crazy2add (m_one, one)));
  print_bool (  3 = crazy2val (crazy2add (m_one, four)));
  print_bool ( -6 = crazy2val (crazy2add (m_one, m_five)));
  print_bool ( -2 = crazy2val (crazy2add (m_one, m_one)));

  print_bool (crazy2val (crazy2add (ONE (ONE (ONE NIL)), ONE (ONE (ONE NIL)))) = 14);
  print_bool (crazy2val (crazy2add (MONE (MONE (MONE NIL)), MONE (MONE (MONE NIL)))) = -14);
  print_bool (crazy2val (crazy2add (ZERO (ZERO (ZERO NIL)), MONE (ONE (ZERO (ZERO NIL))))) = 1);
  print_bool (crazy2val (crazy2add (ONE (ONE (ONE NIL)), ONE (MONE (ZERO (ONE (MONE NIL)))))) = -2);
  print_bool (crazy2val (crazy2add (ONE (ONE (ONE NIL)), ONE (MONE (MONE (ONE (ONE NIL)))))) = 26);
  print_bool (crazy2val (crazy2add (ZERO (ZERO (ZERO NIL)), ONE (MONE (ZERO (ONE (MONE NIL)))))) = -9);
  print_bool (crazy2val (crazy2add (ONE (MONE (ZERO (ONE (MONE NIL)))), MONE (ONE (ZERO (MONE (ONE NIL)))))) = 0);
  print_bool (crazy2val (crazy2add (ZERO (ONE (MONE (ZERO (ONE (MONE NIL))))), ONE (MONE (MONE (ONE (ONE NIL)))))) = 1);
  print_bool (crazy2val (crazy2add (ONE (MONE (ZERO (ONE (MONE NIL)))), ZERO (ONE (MONE (ZERO (ONE (MONE NIL))))))) = -27);
  print_bool (crazy2val (crazy2add (ZERO (ZERO (ZERO (ZERO (ZERO (ZERO (ONE NIL)))))), ZERO (ZERO (ZERO (ZERO (ZERO (ZERO (ONE NIL)))))))) = 128);
;;

let result = crazy2add (MONE (ZERO (ZERO (ZERO NIL))), (ZERO (ONE (ONE (ZERO (MONE (ONE (ONE (ZERO (ONE (ZERO (MONE NIL))))))))))));;