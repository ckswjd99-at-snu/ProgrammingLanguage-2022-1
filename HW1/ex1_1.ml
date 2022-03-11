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
