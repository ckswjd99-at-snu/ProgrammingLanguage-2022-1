open Ex2_6

module ValidIntListQ = (IntListQ : Queue)

let q1 = IntListQ.emptyQ
let q2 = IntListQ.enQ (q1, [1])
let q3 = IntListQ.enQ (q2, [2])
let q4 = IntListQ.enQ (q3, [3])
let x1, q5 = IntListQ.deQ q4
let q6 = IntListQ.enQ (q5, [4])
let q7 = IntListQ.enQ (q6, [5])
let q8 = IntListQ.enQ (q7, [6])
let x2, q9 = IntListQ.deQ q8
let x3, qa = IntListQ.deQ q9
let x4, qb = IntListQ.deQ qa
let x5, qc = IntListQ.deQ qb
let x6, qd = IntListQ.deQ qc

let _=
  let _ = Printf.printf("ex2-6: queue = 2 * stack\n") in
  let print_bool x = print_endline (string_of_bool x) in

  print_bool (fst (IntListQ.deQ q2) = [1]);
  print_bool (fst (IntListQ.deQ q5) = [2]);
  print_bool (fst (IntListQ.deQ (snd (IntListQ.deQ q5))) = [3]);
  print_bool (x1 = [1]);
  print_bool (x2 = [2]);
  print_bool (x3 = [3]);
  print_bool (x4 = [4]);
  print_bool (x5 = [5]);
  print_bool (x6 = [6]);
  print_bool (qd = q1);
  print_bool (try IntListQ.deQ qd = ([], q1) with IntListQ.EMPTY_Q -> true | _ -> false);


let q1 = IntListQ.emptyQ
let q2 = IntListQ.enQ(IntListQ.enQ(IntListQ.enQ(q1, [1]), [2; 3]), [4; 5; 6])
let e1, q3 = IntListQ.deQ q2

let _=
  let print_bool x = print_endline (string_of_bool x) in

  print_bool (q1 = ([], []));
  print_bool (q2 = ([[4; 5; 6]; [2; 3]; [1]], []));
  print_bool (e1 = [1]);
  print_bool (q3 = ([], [[2; 3]; [4; 5; 6]]));

