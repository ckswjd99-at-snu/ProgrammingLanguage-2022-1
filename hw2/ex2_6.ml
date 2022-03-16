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
    let enQ ((target, newtail): queue * element): queue = 
      match target with
      | (stackL, stackR) -> (newtail::stackL, stackR)
    let deQ (target: queue): element * queue = 
      let rec carryStack (targetQueue: queue): queue =
        match targetQueue with
        | ([], stackR) -> targetQueue
        | (headL::tailL, stackR) -> carryStack (tailL, headL::stackR)
      in
      match target with
      | ([], []) -> raise EMPTY_Q
      | (_, []) -> (
        match carryStack target with
        | (newStackL, headR::tailR) -> (headR, (newStackL, tailR))
        | (_, _) -> raise EMPTY_Q
      )
      | (stackL, exHead::stackR) -> (exHead, (stackL, stackR))
  end

let myQ = IntListQ.emptyQ
let yourQ = IntListQ.enQ(myQ, [1])
let (x, restQ) = IntListQ.deQ yourQ
let hisQ = IntListQ.enQ(myQ, [2])