module type Queue =
sig
  type element
  type queue
  exception EMPTY_Q
  val emptyQ: queue
  val enQ: queue * element -> queue 
  val deQ: queue -> element * queue
end

module IntListQ = 
struct
  type element = int list 
  type queue = element list * element list

  exception EMPTY_Q
  let emptyQ = ([], [])
  let enQ (queue, element) =
    let (l, r) = queue in
    (element :: l, r)

  let deQ queue = 
    let (l, r) = queue in
    let (new_l, new_r) =
      match r with
      | [] -> ([], List.rev l)
      | _ -> (l, r)
    in
    match new_r with
    | x :: xs -> (x, (new_l, xs))
    | _ -> raise EMPTY_Q
end
