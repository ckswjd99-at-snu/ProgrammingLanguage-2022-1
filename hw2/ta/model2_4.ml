type heap = 
  | EMPTY 
  | NODE of rank * value * heap * heap
and rank = int
and value = int

exception EmptyHeap

let rec string_of_heap h = 
  match h with
  | EMPTY -> "EMPTY"
  | NODE (r, v, lh, rh) -> Printf.sprintf "NODE (%d, %d, %s, %s)" r v (string_of_heap lh) (string_of_heap rh)

let rank h = 
  match h with
  | EMPTY -> -1
  | NODE(r, _, _, _) -> r

let shake (x, lh, rh) =
  if (rank lh) >= (rank rh)
  then
    NODE (rank rh + 1, x, lh, rh)
  else
    NODE (rank lh + 1, x, rh, lh)

let rec merge (h1, h2) = 
  match (h1, h2) with
  | (_, EMPTY) -> h1
  | (EMPTY, _) -> h2
  | (NODE (_, v1, lh1, rh1), NODE (_, v2, lh2, rh2)) ->
    let (x, lh, rh) = 
      if v1 > v2 then (v2, h1, merge (lh2, rh2)) else (v1, merge (lh1, rh1), h2)
    in
    shake (x, lh, rh)

let insert (x, h) = merge (h, NODE(0, x, EMPTY, EMPTY))

let findMin h =
   match h with 
   | EMPTY -> raise EmptyHeap 
   | NODE(_, x, _, _) -> x

let deleteMin h = 
  match h with 
  | EMPTY -> raise EmptyHeap 
  | NODE(_ ,x, lh, rh) -> merge (lh, rh)
