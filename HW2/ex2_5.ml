exception NOMOVE of string

type item = string
type tree = 
  | LEAF of item
  | NODE of tree list

type zipper = 
  | TOP
  | HAND of tree list * zipper * tree list

type location = LOC of tree * zipper

let goLeft (loc: location): location = 
  match loc with
  | LOC (t, TOP) -> raise (NOMOVE "left of top")
  | LOC (t, HAND(l::left, up, right)) -> LOC (l, HAND (left, up, t::right))
  | LOC (t, HAND([], up, right)) -> raise (NOMOVE "left of first")

let goRight (loc: location): location =
  match loc with
  | LOC (t, TOP) -> raise (NOMOVE "right of top")
  | LOC (t, HAND(left, up, r::right)) -> LOC (r, HAND (t::left, up, right))
  | LOC (t, HAND(left, up, [])) -> raise (NOMOVE "right of last")

let goUp (loc: location): location =
  match loc with
  | LOC (t, TOP) -> raise (NOMOVE "top of top")
  | LOC (t, HAND(left, up, right)) -> LOC(NODE (List.rev left@(t::right)), up)

let goDown (loc: location): location = 
  match loc with
  | LOC (LEAF _, up) -> raise (NOMOVE "down of leaf")
  | LOC (NODE [], up) -> raise (NOMOVE "down of node with no children")
  | LOC (NODE (childrenHead::childrenTail), up) -> LOC (childrenHead, HAND ([], up, childrenTail))
