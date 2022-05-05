type require = id * (cond list)
and cond = 
| Items of gift list
| Same of id
| Common of cond * cond
| Except of cond * gift list
and gift = int
and id = A | B | C | D | E

let commonSet (setList1, setList2) = 
  let inList2 target = List.exists (fun x -> x = target) setList2 in
  List.filter inList2 setList1

let substractSet (setList1, setList2) =
  let notInList2 target = not (List.exists (fun x -> x = target) setList2) in
  List.filter notInList2 setList1

let unionSet (setList1, setList2) = 
  List.sort_uniq (fun x y -> x - y) (setList1 @ setList2)


let rec eval (giftCombi, condition) = 
  match condition with
  | Items giftList -> giftList
  | Same id -> (
    let giftForId = List.find (fun (key, _) -> key = id) giftCombi in
    match giftForId with
    | (key, value) -> value
  )
  | Common (subcond1, subcond2) -> (
    let value1 = eval (giftCombi, subcond1) in
    let value2 = eval (giftCombi, subcond2) in
    commonSet (value1, value2)
  )
  | Except (subcond, exceptlist) -> (
    let value = eval (giftCombi, subcond) in
    substractSet (value, exceptlist)
  )


let shoppingList (reqs : require list) : (id * gift list) list =
  let rec calcGiftList (giftCombi, env, conditions) = 
    match conditions with
    | [] -> giftCombi
    | condHead::condTail -> (
      let newGiftCombi = unionSet ((eval (env, condHead)), giftCombi) in
      calcGiftList (newGiftCombi, env, condTail)
    )
  in
  let rec calcGiftCombi (giftCombi, reqs) =
    let giftCombi' = List.map (fun (id, conditions) -> (id, (calcGiftList ([], giftCombi, conditions)))) reqs in
    if giftCombi = giftCombi' then giftCombi' else calcGiftCombi (giftCombi', reqs)
  in
  let initGiftCombi = [(A,[]); (B,[]); (C,[]); (D,[]); (E,[])] in
  let reqs' = 
    List.map (
        fun (target, _) -> 
          if (List.find_opt (fun (id, _) -> id = target) reqs) = None 
          then (target, []) 
          else (List.find (fun (id, _) -> id = target) reqs) 
      )
      initGiftCombi 
  in
  calcGiftCombi (initGiftCombi, reqs')
