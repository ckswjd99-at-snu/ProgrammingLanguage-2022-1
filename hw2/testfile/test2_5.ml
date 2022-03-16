open Ex2_5

let empty = LOC (NODE [], TOP)
let loc = LOC (LEAF "*", HAND ([LEAF "c"], HAND ([LEAF "+"; NODE [LEAF "a"; LEAF "*"; LEAF "b"]], TOP, []), [LEAF "d"])) 
let loc1 = goLeft loc
let loc2 = goUp loc
let loc3 = goRight loc
let loc4 = goLeft (goLeft loc2)
let loc5 = goRight (goDown loc4)

let _=
  let _ = Printf.printf("ex2-5: zipper\n") in
  let print_bool x = print_endline (string_of_bool x) in

  print_bool (try goLeft empty = empty with NOMOVE _ -> true | _ -> false)
  print_bool (try goRight empty = empty with NOMOVE _ -> true | _ -> false)
  print_bool (try goUp empty = empty with NOMOVE _ -> true | _ -> false)
  print_bool (try goDown empty = empty with NOMOVE _ -> true | _ -> false)

  print_bool (try goDown loc = empty with NOMOVE _ -> true | _ -> false)

  print_bool (loc1 = LOC (LEAF "c", HAND ([], HAND ([LEAF "+"; NODE [LEAF "a"; LEAF "*"; LEAF "b"]], TOP, []), [LEAF "*"; LEAF "d"])))
  print_bool (try goLeft loc1 = empty with NOMOVE _ -> true | _ -> false)
  print_bool (goRight loc1 = loc)
  print_bool (try goDown loc1 = empty with NOMOVE _ -> true | _ -> false)

  print_bool (loc2 = LOC (NODE [LEAF "c"; LEAF "*"; LEAF "d"], HAND ([LEAF "+"; NODE [LEAF "a"; LEAF "*"; LEAF "b"]], TOP, [])))
  print_bool (goLeft loc2 = LOC (LEAF "+", HAND ([NODE [LEAF "a"; LEAF "*"; LEAF "b"]], TOP, [NODE [LEAF "c"; LEAF "*"; LEAF "d"]])))
  print_bool (goLeft (goLeft loc2) = LOC (NODE [LEAF "a"; LEAF "*"; LEAF "b"], HAND ([], TOP, [LEAF "+"; NODE [LEAF "c"; LEAF "*"; LEAF "d"]])))
  print_bool (try goLeft (goLeft (goLeft loc2)) = empty with NOMOVE _ -> true | _ -> false)
  print_bool (try goRight loc2 = empty with NOMOVE _ -> true | _ -> false)
  print_bool (goUp loc1 = loc2)
  print_bool (goUp loc2 = LOC (NODE [NODE [LEAF "a"; LEAF "*"; LEAF "b"]; LEAF "+"; NODE [LEAF "c"; LEAF "*"; LEAF "d"]], TOP))
  print_bool (try goLeft (goUp loc2) = empty with NOMOVE _ -> true | _ -> false)
  print_bool (try goRight (goUp loc2) = empty with NOMOVE _ -> true | _ -> false)
  print_bool (try goUp (goUp loc2) = empty with NOMOVE _ -> true | _ -> false)
  print_bool (goDown loc2 = loc1)

  print_bool (loc3 = LOC (LEAF "d", HAND ([LEAF "*"; LEAF "c"], HAND ([LEAF "+"; NODE [LEAF "a"; LEAF "*"; LEAF "b"]], TOP, []), [])))
  print_bool (goLeft loc3 = loc)
  print_bool (try goRight loc3 = empty with NOMOVE _ -> true | _ -> false)
  print_bool (goUp loc3 = loc2)
  print_bool (try goDown loc3 = empty with NOMOVE _ -> true | _ -> false)

  print_bool (loc4 = LOC (NODE [LEAF "a"; LEAF "*"; LEAF "b"], HAND ([], TOP, [LEAF "+"; NODE [LEAF "c"; LEAF "*"; LEAF "d"]])))
  print_bool (loc5 = LOC (LEAF "*", HAND ([LEAF "a"], HAND ([], TOP, [LEAF "+"; NODE [LEAF "c"; LEAF "*"; LEAF "d"]]), [LEAF "b"])))