(*
 * SNU 4190.310 Programming Languages 2022 Spring
 *  K-- Interpreter Skeleton Code
 *)

(* Location Signature *)
module type LOC =
sig
  type t
  val base : t
  val equal : t -> t -> bool
  val diff : t -> t -> int
  val increase : t -> int -> t
end

module Loc : LOC =
struct
  type t = Location of int
  let base = Location(0)
  let equal (Location(a)) (Location(b)) = (a = b)
  let diff (Location(a)) (Location(b)) = a - b
  let increase (Location(base)) n = Location(base+n)
end

(* Memory Signature *)
module type MEM = 
sig
  type 'a t
  exception Not_allocated
  exception Not_initialized
  val empty : 'a t (* get empty memory *)
  val load : 'a t -> Loc.t  -> 'a (* load value : Mem.load mem loc => value *)
  val store : 'a t -> Loc.t -> 'a -> 'a t (* save value : Mem.store mem loc value => mem' *)
  val alloc : 'a t -> Loc.t * 'a t (* get fresh memory cell : Mem.alloc mem => (loc, mem') *)
end

(* Environment Signature *)
module type ENV =
sig
  type ('a, 'b) t
  exception Not_bound
  val empty : ('a, 'b) t (* get empty environment *)
  val lookup : ('a, 'b) t -> 'a -> 'b (* lookup environment : Env.lookup env key => content *)
  val bind : ('a, 'b) t -> 'a -> 'b -> ('a, 'b) t  (* id binding : Env.bind env key content => env'*)
end

(* Memory Implementation *)
module Mem : MEM =
struct
  exception Not_allocated
  exception Not_initialized
  type 'a content = V of 'a | U
  type 'a t = M of Loc.t * 'a content list
  let empty = M (Loc.base,[])

  let rec replace_nth = fun l n c -> 
    match l with
    | h::t -> if n = 1 then c :: t else h :: (replace_nth t (n - 1) c)
    | [] -> raise Not_allocated

  let load (M (boundary,storage)) loc =
    match (List.nth storage ((Loc.diff boundary loc) - 1)) with
    | V v -> v 
    | U -> raise Not_initialized

  let store (M (boundary,storage)) loc content =
    M (boundary, replace_nth storage (Loc.diff boundary loc) (V content))

  let alloc (M (boundary,storage)) = 
    (boundary, M (Loc.increase boundary 1, U :: storage))
end

(* Environment Implementation *)
module Env : ENV=
struct
  exception Not_bound
  type ('a, 'b) t = E of ('a -> 'b)
  let empty = E (fun x -> raise Not_bound)
  let lookup (E (env)) id = env id
  let bind (E (env)) id loc = E (fun x -> if x = id then loc else env x)
end

(*
 * K-- Interpreter
 *)
module type KMINUS =
sig
  exception Error of string
  type id = string
  type exp =
    | NUM of int | TRUE | FALSE | UNIT
    | VAR of id
    | ADD of exp * exp
    | SUB of exp * exp
    | MUL of exp * exp
    | DIV of exp * exp
    | EQUAL of exp * exp
    | LESS of exp * exp
    | NOT of exp
    | SEQ of exp * exp            (* sequence *)
    | IF of exp * exp * exp       (* if-then-else *)
    | WHILE of exp * exp          (* while loop *)
    | LETV of id * exp * exp      (* variable binding *)
    | ASSIGN of id * exp          (* assgin to variable *)
    | READ of id
    | WRITE of exp
    
  type program = exp
  type memory
  type env
  type value =
    | Num of int
    | Bool of bool
    | Unit
  val emptyMemory : memory
  val emptyEnv : env
  val run : memory * env * program -> value
end

module K : KMINUS =
struct
  exception Error of string

  type id = string
  type exp =
    | NUM of int | TRUE | FALSE | UNIT
    | VAR of id
    | ADD of exp * exp
    | SUB of exp * exp
    | MUL of exp * exp
    | DIV of exp * exp
    | EQUAL of exp * exp
    | LESS of exp * exp
    | NOT of exp
    | SEQ of exp * exp            (* sequence *)
    | IF of exp * exp * exp       (* if-then-else *)
    | WHILE of exp * exp          (* while loop *)
    | LETV of id * exp * exp      (* variable binding *)
    | ASSIGN of id * exp          (* assgin to variable *)
    | READ of id
    | WRITE of exp

  type program = exp

  type value =
    | Num of int
    | Bool of bool
    | Unit
    
  type memory = value Mem.t
  type env = (id, env_entry) Env.t
  and  env_entry = Loc.t 

  let emptyMemory = Mem.empty
  let emptyEnv = Env.empty

  let value_int v =
    match v with
    | Num n -> n
    | _ -> raise (Error "TypeError : not int")

  let value_bool v =
    match v with
    | Bool b -> b
    | _ -> raise (Error "TypeError : not bool")

  let value_unit v =
    match v with
    | Unit -> ()
    | _ -> raise (Error "TypeError : not unit")

  let lookup_env_loc e x =
    try
      Env.lookup e x
    with Env.Not_bound -> raise (Error "Unbound")

  let rec eval : memory -> env -> exp -> (value * memory) = fun mem env e ->
    match e with
    | UNIT -> (Unit, mem)
    | NUM i -> (Num i, mem)
    | TRUE -> (Bool true, mem)
    | FALSE -> (Bool false, mem)
    | VAR id -> (Mem.load mem (lookup_env_loc env id), mem)
    | ADD (e1, e2) -> 
      let v1, mem' = eval mem env e1 in
      let v2, mem'' = eval mem' env e2 in
      (Num (value_int v1 + value_int v2), mem'')
    | SUB (e1, e2) -> 
      let v1, mem' = eval mem env e1 in
      let v2, mem'' = eval mem' env e2 in
      (Num (value_int v1 - value_int v2), mem'')
    | MUL (e1, e2) -> 
      let v1, mem' = eval mem env e1 in
      let v2, mem'' = eval mem' env e2 in
      (Num (value_int v1 * value_int v2), mem'')
    | DIV (e1, e2) -> 
      let v1, mem' = eval mem env e1 in
      let v2, mem'' = eval mem' env e2 in
      (Num (value_int v1 / value_int v2), mem'')
    | READ x -> 
      let v = Num (read_int()) in
      let l = lookup_env_loc env x in
      (v, Mem.store mem l v)
    | WRITE e ->
      let (v, mem') = eval mem env e in
      let n = value_int v in
      let _ = print_endline (string_of_int n) in
      (v, mem')
    | LETV (x, e1, e2) ->
      let (v, mem') = eval mem env e1 in
      let (l, mem'') = Mem.alloc mem' in
      eval (Mem.store mem'' l v) (Env.bind env x l) e2
    | ASSIGN (x, e) ->
      let (v, mem') = eval mem env e in
      let l = lookup_env_loc env x in
      (v, Mem.store mem' l v)
    | LESS (e1, e2) ->
      let v1, mem' = eval mem env e1 in
      let v2, mem'' = eval mem' env e2 in
      (Bool (value_int v1 < value_int v2), mem'')
    | NOT e1 ->
      let v, mem' = eval mem env e1 in
      (Bool (not (value_bool v)), mem')
    | SEQ (e1, e2) ->
      let ( _ , mem') = eval mem env e1 in
      eval mem' env e2
    | IF (e_cond, e_true, e_false) ->
      let (v_cond, mem') = eval mem env e_cond in
      if value_bool v_cond then eval mem' env e_true else eval mem' env e_false
    | WHILE (e_cond, e_body) -> 
      let (v_cond, mem') = eval mem env e_cond in
      if value_bool v_cond then 
        let ( _ , mem'') = eval mem env e_body in
        eval mem'' env (WHILE (e_cond, e_body))
      else
        (Unit, mem')
    | EQUAL (e1, e2) ->
      let v1, mem' = eval mem env e1 in
      let v2, mem'' = eval mem' env e2 in
      let b = 
        match (v1, v2) with
        | (Num n1, Num n2) -> n1 = n2
        | (Bool b1, Bool b2) -> b1 = b2
        | (Unit, Unit) -> true
        | _ -> false
      in
      (Bool b, mem'')

  let run (mem, env, pgm) = 
    let (v, _ ) = eval mem env pgm in
    v
end