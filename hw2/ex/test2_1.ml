open Ex2_1

galculator (SIGMA (INT 1, INT 10, SUB(MUL(X, X), INT 1)));;

float = 375.

galculator (SIGMA (REAL 1., REAL 10., SUB(MUL(X, X), INT 1)));;

float = 375.

galculator (INTEGRAL (REAL 1., REAL 10., SUB(MUL(X, X), INT 1)));;

float = 319.064999999…

galculator (INTEGRAL (REAL 10., REAL 1., SUB(MUL(X, X), INT 1)));;

float = -319.06499999…

galculator (INTEGRAL (REAL 10., SIGMA (REAL 1., REAL 10., SUB(MUL(X, X), INT 1)), SUB(MUL(X, X), INT 1)));;

float = 17556346.1239999793

galculator (INTEGRAL (REAL 10., SIGMA (REAL 1., REAL 10., SUB(MUL(X, X), INT 1)), SUB(MUL(X, X), X)));;

float = 17486504.2639999

galculator (INTEGRAL (ADD (X, REAL 1.), SIGMA (REAL 1., REAL 10., SUB(MUL(X, X), INT 1)), SUB(MUL(X, X), X)));;

let n1 = INT 3
let n2 = REAL 1.2
let n3 = INT (-2)
let n4 = REAL 0.8

let x1 = ADD (X, INT 1)
let x2 = MUL (X, (MUL (INT 2, X)))
let x3 = SUB (MUL (X, X), INT 1)

let s1 = SIGMA (INT 0, INT 1, X)
let s2 = SIGMA (INT 0, X, MUL (MUL (X, X), INT 3))
let s3 = SIGMA (s1, INT 10, s2)

let check_tight : float -> float -> bool =
  fun a b ->
    a -. b < 0.00001 &&  b -. a < 0.00001

let check_loose : float -> float -> bool =
  fun a b ->
    a -. b < 0.5 && b -. a < 0.5

(* Arithmatic *)
let _ = check (fun () -> check_tight (galculator n1) 3.0)
let _ = check (fun () -> check_tight (galculator n2) 1.2)
let _ = check (fun () -> check_tight (galculator n3) (-2.0))
let _ = check (fun () -> check_tight (galculator n4) 0.8)
let _ = check (fun () -> check_tight (galculator (ADD (n1, n2))) 4.2)
let _ = check (fun () -> check_tight (galculator (ADD (ADD (n1, n2), n3))) 2.2)
let _ = check (fun () -> check_tight (galculator (ADD (ADD (n1, n2), n4))) 5.0)
let _ = check (fun () -> check_tight (galculator (SUB (n1, n2))) 1.8)
let _ = check (fun () -> check_tight (galculator (SUB (n4, n3))) 2.8)
let _ = check (fun () -> check_tight (galculator (SUB (SUB (n4, n3), n3))) 4.8)
let _ = check (fun () -> check_tight (galculator (MUL (n1, n2))) 3.6)
let _ = check (fun () -> check_tight (galculator (MUL (ADD (n3, n4), n2))) (-1.44))
let _ = check (fun () -> check_tight (galculator (MUL (n1, (SUB (INT 0, n2))))) (-3.6))
let _ = check (fun () -> check_tight (galculator (DIV (n1, n2))) 2.5)
let _ = check (fun () -> check_tight (galculator (DIV (n4, n3))) (-0.4))
let _ = check (fun () ->
  try check_tight (galculator X) 123.0 with FreeVariable -> true | _ -> false)

(* Sigma *)
let _ = check (fun () -> check_tight (galculator (SIGMA (INT 1, INT 10, REAL 0.5))) 5.0)
let _ = check (fun () -> check_tight (galculator (SIGMA (INT 1, INT 10, X))) 55.0)
let _ = check (fun () -> check_tight (galculator (SIGMA (REAL 1.0, INT 100, x1))) 5150.0)
let _ = check (fun () -> check_tight (galculator (SIGMA (REAL 1.0, REAL 10.1, x2))) 770.0)
let _ = check (fun () -> check_tight (galculator (SIGMA (INT 4, INT 12, MUL ((SUB (X, REAL 1.0)), x1)))) 627.0)
let _ = check (fun () -> check_tight (galculator (SIGMA (INT 4, INT 12, x3))) 627.0)
let _ = check (fun () -> check_tight (galculator s3) 3630.0)
let _ = check (fun () ->
  check_tight (galculator
  (SIGMA
    (SUB (INT 3, REAL 1.0),
    SIGMA (INT 1, INT 3, X),
    SIGMA (X, ADD (X, X), SUB (MUL (INT 2, MUL (X, X)), MUL (REAL 3.0, X))))))
  2015.0)
let _ = check (fun () ->
  check_tight (galculator
  (SIGMA (SIGMA (INT 2, INT 1, X), INT 10,
    (SIGMA (SIGMA (INT (-1), INT 1, X), X,
      (SIGMA (INT 0, X, MUL (X, X))))))))
  3289.0)
let _ = check (fun () ->
  try check_tight (galculator (SIGMA (INT 0, X, X))) 0.0 with FreeVariable -> true | _ -> false)

(* Integral *)
let _ = check (fun () ->
  check_loose (galculator (INTEGRAL (INT 2, REAL 2.05, ADD (X, X)))) 0.0)
let _ = check (fun () ->
  check_loose (galculator (INTEGRAL (REAL (-2.0), REAL (-2.05), DIV (X, X)))) 0.0)
let _ = check (fun () ->
  check_loose (galculator (INTEGRAL (INT 0, INT 2, REAL 0.5))) 0.95)
let _ = check (fun () ->
  check_loose (galculator (INTEGRAL (INT 2, INT 0, REAL (-1.0)))) 1.9) 
let _ = check (fun () ->
  check_loose (galculator (INTEGRAL (INT 0, INT 2, MUL (X, INT 2)))) 3.8)
let _ = check (fun () ->
  check_loose (galculator (INTEGRAL (REAL 1.55, REAL 1.37, X))) (-0.137))
let _ = check (fun () ->
  check_loose (galculator (INTEGRAL (INT 2, INT 0, MUL (X, X)))) (-2.47))
let _ = check (fun () ->
  check_loose (galculator (INTEGRAL (REAL 0.1, INT 1, DIV (INT 100, X)))) 282.896)
let _ = check (fun () ->
  check_loose (galculator (INTEGRAL (REAL 10.0, REAL 1.0, SUB(MUL(X, X), INT 1)))) (-319.065))
let _ = check (fun () ->
  check_loose (galculator (INTEGRAL (INT 1, INT (-1), MUL (MUL (X, X), X)))) 0.0)
let _ = check (fun () ->
  check_loose (galculator (INTEGRAL (INT 1, INT 0, MUL (INT 3, MUL (X, X))))) (-0.855))
let _ = check (fun () ->
  try check_loose (galculator (INTEGRAL (INT 0, MUL (INT 1, X), X))) 0.0 with FreeVariable -> true | _ -> false)
let _ = check (fun () ->
  try check_loose
    (galculator
      (INTEGRAL
        (ADD (X, REAL 1.0),
        SIGMA (REAL 1.0, REAL 10.0, SUB (MUL(X, X), INT 1)),
        SUB(MUL(X, X), X))))
    0.0 with FreeVariable -> true | _ -> false)
let _ = check (fun () ->
  try check_loose
    (galculator
      (SIGMA
        (INTEGRAL (INT 0, MUL (INT 1, INT 0), X),
        INTEGRAL (SUB (INT 1, MUL (INT 2, X)), INT 30, REAL 10.5),
        MUL (X, X))))
    0.0 with FreeVariable -> true | _ -> false)