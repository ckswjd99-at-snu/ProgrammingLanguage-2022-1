(* testcase 12 : higher order function *)

(fn f => f (fn p => (fn a => 1 + a + (p 10)))) (fn ff => ((ff (fn q => ifp (q + 1) then (q + 100) else (q + 200))) 1000) + 10000)
