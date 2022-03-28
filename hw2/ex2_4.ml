exception EmptyHeap

type heap = EMPTY | NODE of rank * value * heap * heap
and rank = int
and value = int

let rank h = match h with
  | EMPTY -> -1
  | NODE (r, _, _, _) -> r

let shake (x, lh, rh) = 
  if (rank lh) >= (rank rh)
  then NODE(rank rh+1, x, lh, rh)
  else NODE(rank lh+1, x, rh, lh)

let rec merge ((heap1, heap2): heap * heap) : heap = 
  match heap1, heap2 with
  | EMPTY, EMPTY -> EMPTY
  | EMPTY, NODE (_, _, _, _) -> heap2
  | NODE (_, _, _, _), EMPTY -> heap1
  | NODE (rank1, val1, heap1_1, heap1_2), NODE (rank2, val2, heap2_1, heap2_2) -> (
    if val1 < val2 then (
      let merged1 = merge (heap1_1, heap1_2) in
      shake (val1, merged1, heap2)
    )
    else (
      let merged2 = merge (heap2_1, heap2_2) in
      shake (val2, heap1, merged2)
    )
  )

let insert (x, h) = merge (h, NODE(0, x, EMPTY, EMPTY))

let findMin h = match h with
  | EMPTY -> raise EmptyHeap
  | NODE (_, x, lh, rh) -> x

let deleteMin h = match h with
  | EMPTY -> raise EmptyHeap
  | NODE (_, x, lh, rh) -> merge (lh, rh)
