type treasure = StarBox | NameBox of string
type key = Bar | Node of key * key
type map = 
| End of treasure
| Branch of map * map
| Guide of string * map
exception IMPOSSIBLE

type rawKey = Bar | Var of int | Node of rawKey * rawKey
type envi = (map * rawKey) list

let exist_env (env : envi) (expr : map) = (
  List.exists (fun (_map, _rawKey) -> _map = expr) env
)

let lookup_env_opt (env : envi) (expr : map) = (
  let (_, result) = List.find (fun (_map, _rawKey) -> _map = expr) env in
  result
)

let rec eval ((env, expr, varnum) : (envi * map * int)) : (envi * int) = 
  match expr with 
  | End StarBox -> (
    if exist_env env expr
    then (env, varnum)
    else ((expr, Bar)::env, varnum)
  )
  | End (NameBox boxName) -> (
    if exist_env env expr
    then (env, varnum)
    else ((expr, Var (varnum+1))::env, varnum+1)
  )
  | Branch (expr1, expr2) -> (
    let (env', varnum') = eval (env, expr2, varnum) in
    let key2 = lookup_env_opt env' expr2 in
    let env'' = (expr, Var (varnum'+1))::env' in
    let env''' = (expr1, Node (key2, Var (varnum'+1)))::env'' in
    eval (env''', expr1, varnum'+1)
  )
  | Guide (boxName, expr1) -> (
    let (env', varnum') = eval (env, expr1, varnum) in
    let (env'', varnum'') = eval (env', (End (NameBox boxName)), varnum') in
    let key1 = lookup_env_opt env'' expr1 in
    let key2 = lookup_env_opt env'' (End (NameBox boxName)) in
    ((expr, Node (key1, key2))::env'', varnum'')
  )

let rec collect_rules set rules expr = (
  match rules with
  | [] -> set
  | (_map, _rawKey)::ruleTail -> (
    if _map = expr
    then collect_rules (_rawKey::set) ruleTail expr
    else collect_rules set ruleTail expr
  )
)

let find_sets rules = List.map (fun (_map, _) -> (_map, (collect_rules [] rules _map))) rules

let find_nested set = List.filter (fun (_map, _rawKeyList) -> List.length _rawKeyList > 1) set

let remove_dupl set = (
  let rec remove_iter set = 
    match set with
    | [] -> set
    | h::t -> if List.mem h t then remove_iter t else h::(remove_iter t)
  in
  remove_iter set
)

let remove_map set = List.map (fun (_, _rawKey) -> _rawKey) set

let is_box (_map, _) = (
  match _map with
  | End _ -> true
  | _ -> false
)

let only_namebox_rule rules = List.filter is_box rules



(* Let's find expanding paths *)
let rec traverse _map _name = (
  match _map with
  | Node (map1, map2) -> (traverse map1 _name) || (traverse map2 _name)
  | Var name -> name = _name
  | Bar -> false
)

let rec match_key rawKey1 rawKey2 = (
  match rawKey1, rawKey2 with
  | (Var name1, Var name2) -> if name1 = name2 then [] else [(rawKey1, rawKey2)]
  | (Var name1, Node (map21, map22)) -> (
    if traverse rawKey2 name1 then raise IMPOSSIBLE else [(rawKey1, rawKey2)]
  )
  | (Node (map11, map12), Var name2) -> (
    if traverse rawKey1 name2 then raise IMPOSSIBLE else [(rawKey2, rawKey1)]
  )
  | (Node (map11, map12), Node (map21, map22)) -> (match_key map11 map21) @ (match_key map21 map22)
  | (Var name1, Bar) | (Bar, Var name1) -> [(Var name1, Bar)]
  | _ -> raise IMPOSSIBLE
)

let rec resolve_matches nested_list = 
  match nested_list with
  | [] -> []
  | nestedHead::nestedTail -> (
    let rec resolve_match nested = 
      match nested with
      | [] -> []
      | nHead::nTail -> (
        List.fold_left (fun alreadyList y -> alreadyList @ (match_key nHead y)) [] nTail
      ) @ (resolve_match nTail)
    in
    resolve_match nestedHead
  ) @ (resolve_matches nestedTail)


(* make key set *)
let rec replace_variable = fun (Var fromName, (toKey : rawKey)) (_rawKey : rawKey) : rawKey -> (
  match _rawKey with
  | Bar -> Bar
  | Var _name -> if _name = fromName then toKey else _rawKey
  | Node (_rk1, _rk2) -> Node ((replace_variable ((Var fromName), toKey) _rk1), (replace_variable ((Var fromName), toKey) _rk2))
)

let melt_match nameboxes match_rule =
  List.map (fun (_expr, _rk) -> (_expr, replace_variable match_rule _rk)) nameboxes

let melt_matches nameboxes match_rules =
  List.fold_left (fun already x -> (melt_match nameboxes x)) nameboxes match_rules

let into_bar (nameboxes : (map * rawKey) list) : key list = (
  let rec barize _rawKey : key = 
    match _rawKey with
    | Bar | Var _ -> Bar
    | Node (_rk1, _rk2) -> Node (barize _rk1, barize _rk2)
  in
  List.map (fun (_map, x) -> barize x) nameboxes
)

let getReady (_map : map) : key list = (
  let resultEnv, usedVar = eval ([], _map, -1) in

  (* Find nested variables *)
  let nesteds = remove_map (remove_dupl (find_nested (find_sets resultEnv))) in

  (* Find only namebox rules *)
  let namebox_rules = only_namebox_rule resultEnv in

  (* Find important matches *)
  let resolved_matches = resolve_matches nesteds in

  (* MELTDOWN! *)
  let melted = melt_matches namebox_rules resolved_matches in

  (* barize all *)
  let result = remove_dupl (into_bar melted) in

  result
)
