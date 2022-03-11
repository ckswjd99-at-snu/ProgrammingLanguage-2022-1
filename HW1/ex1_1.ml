let rec merge ((a, b) : int list * int list) : int list = 
  match a with
  | [] -> b
  | headA::tailA -> (
    match b with
    | [] -> a
    | headB::tailB -> (
      if headA > headB then headA::(merge (tailA, b))
      else headB::(merge (a, tailB))
    )
  )
;;


(* let listA = [6;4;2];; let listB = [5;3;1];; let merged = merge (listA, listB);; open Printf;; let () = List.iter (printf "%d ") merged;; *)
