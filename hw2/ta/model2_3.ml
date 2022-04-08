type ae = 
  | CONST of int
  | VAR of string
  | POWER of string * int
  | TIMES of ae list
  | SUM of ae list

exception InvalidArgument

let rec diff (e, var) = 
  match e with
  | CONST i -> CONST 0
  | VAR x -> if x = var then CONST 1 else CONST 0
  | POWER (x, i) -> if x = var then TIMES [CONST i; POWER (x, i - 1)] else CONST 0
  | TIMES e_list -> 
    let len = List.length e_list in
    if len = 0 
    then raise InvalidArgument
    else
      let rec times_diff i results = 
        if i >= len 
        then results
        else
          let new_result = TIMES (List.mapi (fun index e -> if i = index then diff (e, var) else e) e_list) in
          times_diff (i + 1) (new_result :: results)
      in
      SUM (times_diff 0 [])
  | SUM e_list -> 
    match e_list with
    | [] -> raise InvalidArgument
    | _ -> SUM (List.map (fun e -> diff (e, var)) e_list)
