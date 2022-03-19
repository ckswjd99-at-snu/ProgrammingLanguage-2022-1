open Ex2_4

open Printf

let _ = printf "ex2-4: leftist heap\n"

let correct_symbol =
  "\x1b[32m✓\x1b[0m"

let wrong_symbol =
  "\x1b[31m✗\x1b[0m"

let test_testcase num tc runner string_of_tc =
  if runner tc then printf "%s Test %d\n%!" correct_symbol num
  else
    let (tc_s, ans_s, output_s) = string_of_tc tc in
    printf "%s Test %d: %s\n  answer: %s, output: %s\n%!" wrong_symbol num tc_s ans_s output_s

let test_testcase2 num tc runner result_of_tc print_res =
  if runner tc then printf "%s Test %d\n%!" correct_symbol num
  else
    let (tc_s, ans, out) = result_of_tc tc in
    let _ = printf "%s Test %d: %s\n  answer: %!" wrong_symbol num tc_s in
    let _ = print_res ans in
    let _ = print_string ", output: " in
    let _ = print_res out in
    print_newline ()

let test_exercise tcs runner string_of_tc =
  let rec test_exercise_ tcnum tcs runner =
    match tcs with
    | [] -> ()
    | tc::tcs' ->
        let _ = test_testcase tcnum tc runner string_of_tc
        in test_exercise_ (tcnum+1) tcs' runner
  in test_exercise_ 1 tcs runner

let test_exercise2 tcs runner result_of_tc print_res =
  let rec test_exercise_ tcnum tcs runner =
    match tcs with
    | [] -> ()
    | tc::tcs' ->
        let _ = test_testcase2 tcnum tc runner result_of_tc print_res
        in test_exercise_ (tcnum+1) tcs' runner
  in test_exercise_ 1 tcs runner

let summary_exercise tcs runner =
  let total = List.length tcs in
  let passed = List.length (List.filter runner tcs) in
  printf "%s Passed %d/%d Cases\n" (if passed = total then correct_symbol else wrong_symbol) passed total

let wrapper a1 a2 a3 =
  if Array.length Sys.argv = 1 then
    test_exercise a1 a2 a3
  else
    summary_exercise a1 a2

let wrapper2 a1 a2 a3 a4 =
  if Array.length Sys.argv = 1 then
    test_exercise2 a1 a2 a3 a4
  else
    summary_exercise a1 a2

module type TestEx =
  sig
    type testcase

    val testcases: testcase list
    val runner: testcase -> bool
    (* str_of_input: string * string_of_ans: string * string_of_output: string *)
    val string_of_tc: testcase -> string * string * string
  end

let char_list_of_string str =
  let rec char_list_of_string_ str idx lim l =
    if idx < lim then char_list_of_string_ str (idx+1) lim (str.[idx]::l)
    else l
  in char_list_of_string_ str 0 (String.length str) []

let string_of_list string_of_elem ls =
  "[" ^ (String.concat "; " (List.map string_of_elem ls)) ^ "]"

let value : heap -> int = fun (h) ->
  match h with
  | EMPTY -> -987654321
  | NODE (_,v,_,_) -> v

let rec check_structure : heap -> bool = fun (h) ->
  match h with
  | EMPTY -> true
  | NODE (r,v,lh,rh) -> begin
    (check_structure lh) && (check_structure rh) && (rank lh >= rank rh)
  end

module TestEx3: TestEx =
  struct
    type testcase =
      | SEQ of seq list
    and seq =
      | INSERT of int
      | FINDMIN of int
      | FINDMIN_EMPTY
      | DELETEMIN
      | DELETEMIN_EMPTY

    let testcases =
      [ 
        SEQ [INSERT 1;  INSERT 2; FINDMIN 1; DELETEMIN; FINDMIN 2;];
        SEQ [ DELETEMIN_EMPTY; ];
        SEQ [ DELETEMIN_EMPTY; DELETEMIN_EMPTY; DELETEMIN_EMPTY; DELETEMIN_EMPTY; DELETEMIN_EMPTY;];
        SEQ [ FINDMIN_EMPTY; ];
        SEQ [ FINDMIN_EMPTY; FINDMIN_EMPTY; FINDMIN_EMPTY; FINDMIN_EMPTY; FINDMIN_EMPTY; FINDMIN_EMPTY; ];
        SEQ [INSERT 1;  INSERT 2; FINDMIN 1; DELETEMIN; FINDMIN 2; DELETEMIN; FINDMIN_EMPTY; DELETEMIN_EMPTY; ];
        SEQ [INSERT 1; INSERT 1; INSERT 1; FINDMIN 1; DELETEMIN; FINDMIN 1; DELETEMIN; FINDMIN 1; DELETEMIN; FINDMIN_EMPTY; ];
        SEQ [INSERT 10; INSERT 5; INSERT 20; INSERT 30; INSERT 0; INSERT 0; FINDMIN 0; DELETEMIN; FINDMIN 0; DELETEMIN
            ;FINDMIN 5; DELETEMIN;FINDMIN 10; DELETEMIN; FINDMIN 20; DELETEMIN; FINDMIN 30; DELETEMIN;FINDMIN_EMPTY; DELETEMIN_EMPTY;];
        SEQ [INSERT 1; INSERT 2; INSERT 3; FINDMIN 1; DELETEMIN; FINDMIN 2; DELETEMIN; FINDMIN 3; DELETEMIN;];
        SEQ [INSERT 3; INSERT 2; INSERT 1; FINDMIN 1; DELETEMIN; FINDMIN 2; DELETEMIN; FINDMIN 3; DELETEMIN;];
        SEQ [INSERT 5; INSERT 5; INSERT 5; FINDMIN 5; DELETEMIN; INSERT 4; FINDMIN 4; DELETEMIN; INSERT 1; FINDMIN 1;];
      ]

    let runner tc =
      let rec runner_ : (seq list) * heap -> bool = fun (l,h) ->
        if (check_structure h) == false then false
        else
        match l with
        | [] -> true
        | (head::tc') -> begin
            match head with
            | INSERT x -> runner_ (tc', insert (x,h))
            | FINDMIN x ->
                let y = findMin h in
                if x = y then runner_ (tc', h)
                else false
            | FINDMIN_EMPTY ->
                let _ = try Some (findMin h) with EmptyHeap -> None in
                true
            | DELETEMIN -> runner_ (tc', deleteMin h)
            | DELETEMIN_EMPTY ->
                let _ = try Some (deleteMin h) with EmptyHeap -> None in
                true
        end
      in
      match tc with
      | SEQ l -> (runner_ (l,EMPTY))

    let string_of_tc tc =
      let rec string_of_seqs : (seq list) * heap -> (string*string*string) = fun (seqs,h) ->
        if (check_structure h) == false then
          ("", "", "Invalid leftist heap structure! (right child's rank is greater than left)" )
        else
        match seqs with
        | [] -> ("", "", "")
        | (head::seqs') ->
            match head with
            | INSERT x -> begin
                let (s, ans, out) = string_of_seqs (seqs',insert (x, h)) in
                ("\n  insert " ^ (string_of_int x)  ^ s, ans, out)
            end

            | FINDMIN x -> begin
                let y = findMin h in
                let (s,ans,out) = string_of_seqs (seqs',h) in
                if x = y then
                  ("\n  findMin: Expected " ^ string_of_int x ^ ", Your output " ^ string_of_int y ^ s, ans, out)
                else
                  ("\n  " ^ wrong_symbol ^ "findMin: Expected " ^ string_of_int x ^ ", Your output " ^ string_of_int y ^ s, ans, out)
            end
            
            | FINDMIN_EMPTY -> begin
                let res = try Some (findMin h) with EmptyHeap -> None in
                match res with
                | Some (y) -> ("\n  " ^ wrong_symbol ^ " findMin", "Exception EmptyHeap", string_of_int y)
                | None ->
                    let (s, ans, out) = string_of_seqs (seqs',h)
                    in ("\n  " ^ correct_symbol ^ "findMin = Exception EmptyHeap" ^ s, ans, out)
            end

            | DELETEMIN ->
                let (s, ans, out) = string_of_seqs (seqs',deleteMin (h)) in
                ("\n  deleteMin()" ^ s, ans, out)
            | DELETEMIN_EMPTY ->
                let res = try Some (deleteMin h) with EmptyHeap -> None in
                match res with
                | Some (y) -> ("\n  " ^ wrong_symbol ^ " deleteMin ", "Exception EmptyHeap", "Non-empty heap")
                | None ->
                    let (s, ans, out) = string_of_seqs (seqs',h)
                    in ("\n  " ^ correct_symbol ^ "deleteMin = Exception EmptyHeap" ^ s, ans, out)
      in
      match tc with
      | SEQ seqs -> string_of_seqs (seqs,EMPTY)
  end

open TestEx3
let _ = wrapper testcases runner string_of_tc