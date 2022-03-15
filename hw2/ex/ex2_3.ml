exception EmptyHeap

type heap = EMPTY | NODE of rank * value * heap * heap
and rank = int
and value = int

let shake (x, lh, rh) = 
  if (rank lh) >= (rank rh)
  then NODE(rank rh+1, x, lh, rh)
  else NODE(rank lh+1, x, rh, lh)

let merge ((heap1, heap2): heap * heap) : heap

let rank h = match h with
  | EMPTY -> -1
  | NODE (r, _, _, _) -> r
let insert (x, h) = merge (h, NODE(0, x, EMPTY, EMPTY))
let findMin h = match h with
  | EMPTY -> raise EmptyHeap
  | NODE (_, x, lh, rh) -> merge (lh, rh)