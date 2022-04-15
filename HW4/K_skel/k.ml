(*
 * SNU 4190.310 Programming Languages 2018 Fall
 *  K- Interpreter Skeleton Code
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
 * K- Interpreter
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
    | LETF of id * id list * exp * exp (* procedure binding *)
    | CALLV of id * exp list      (* call by value *)
    | CALLR of id * id list       (* call by referenece *)
    | RECORD of (id * exp) list   (* record construction *)
    | FIELD of exp * id           (* access record field *)
    | ASSIGN of id * exp          (* assgin to variable *)
    | ASSIGNF of exp * id * exp   (* assign to record field *)
    | READ of id
    | WRITE of exp
    
  type program = exp
  type memory
  type env
  type value =
    | Num of int
    | Bool of bool
    | Unit
    | Record of (id -> Loc.t)
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
    | LETF of id * id list * exp * exp (* procedure binding *)
    | CALLV of id * exp list      (* call by value *)
    | CALLR of id * id list       (* call by referenece *)
    | RECORD of (id * exp) list   (* record construction *)
    | FIELD of exp * id           (* access record field *)
    | ASSIGN of id * exp          (* assgin to variable *)
    | ASSIGNF of exp * id * exp   (* assign to record field *)
    | READ of id
    | WRITE of exp

  type program = exp

  type value =
    | Num of int
    | Bool of bool
    | Unit
    | Record of (id -> Loc.t)
    
  type memory = value Mem.t
  type env = (id, env_entry) Env.t
  and  env_entry = Addr of Loc.t | Proc of id list * exp * env

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

  let value_record v =
    match v with
    | Record r -> r
    | _ -> raise (Error "TypeError : not record")

  let lookup_env_loc e x =
    try
      (match Env.lookup e x with
      | Addr l -> l
      | Proc _ -> raise (Error "TypeError : not addr")) 
    with Env.Not_bound -> raise (Error "Unbound")

  let lookup_env_proc e f =
    try
      (match Env.lookup e f with
      | Addr _ -> raise (Error "TypeError : not proc") 
      | Proc (id_list, exp, env) -> (id_list, exp, env))
    with Env.Not_bound -> raise (Error "Unbound")

  let rec eval mem env e =
    match e with
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
      eval (Mem.store mem'' l v) (Env.bind env x (Addr l)) e2
    | ASSIGN (x, e) ->
      let (v, mem') = eval mem env e in
      let l = lookup_env_loc env x in
      (v, Mem.store mem' l v)
    | NUM integer -> (Num integer, mem)
    | TRUE -> (Bool true, mem)
    | FALSE -> (Bool false, mem)
    | UNIT -> (Unit, mem)
    | VAR identifier -> 
      let l = lookup_env_loc env identifier in
      let v = Mem.load mem l in
      (v, mem)
    | RECORD (keyValList: (id * exp) list) -> 
    (
      match keyValList with 
      | [] -> (Unit, mem)
      | _ -> (
        let rec makeRecord (record, keyValList, memory) = (
          match keyValList with
          | [] -> (record, memory)
          | (key, expr)::keyValTail -> (
            let (v, memory') = eval memory env expr in
            let (l, memory'') = Mem.alloc memory' in
            let memory''' = Mem.store memory'' l v in
            let record' identifier = if identifier = key then l else (record identifier) in
            let (record'', memory'''') = makeRecord (record', keyValTail, memory''') in
            (record'', memory'''')
          )
        )
        in
        let (record, mem') = makeRecord ((fun x -> (raise (Error "record no key"))), keyValList, mem) in
        (Record record, mem')
      )
    )
    | ADD (expr1, expr2) ->
      let (num1, mem') = eval mem env expr1 in
      let (num2, mem'') = eval mem' env expr2 in
      (Num ((value_int num1) + (value_int num2)), mem'')
    | SUB (expr1, expr2) ->
      let (num1, mem') = eval mem env expr1 in
      let (num2, mem'') = eval mem' env expr2 in
      (Num ((value_int num1) - (value_int num2)), mem'')
    | MUL (expr1, expr2) ->
      let (num1, mem') = eval mem env expr1 in
      let (num2, mem'') = eval mem' env expr2 in
      (Num ((value_int num1) * (value_int num2)), mem'')
    | DIV (expr1, expr2) ->
      let (num1, mem') = eval mem env expr1 in
      let (num2, mem'') = eval mem' env expr2 in
      (Num ((value_int num1) / (value_int num2)), mem'')
    | EQUAL (expr1, expr2) -> (
      let (v1, mem') = eval mem env expr1 in
      let (v2, mem'') = eval mem' env expr2 in
      match v1, v2 with
      | (Num n1, Num n2) -> (Bool (n1 = n2), mem'')
      | (Bool b1, Bool b2) -> (Bool (b1 = b2), mem'')
      | (Unit, Unit) -> (Bool true, mem'')
      | _ -> (Bool false, mem'')
    )
    | LESS (expr1, expr2) ->
      let (v1, mem') = eval mem env expr1 in
      let (v2, mem'') = eval mem' env expr2 in
      let n1 = value_int v1 in
      let n2 = value_int v2 in
      (Bool (n1 < n2), mem'')
    | NOT expr ->
      let (v, mem') = eval mem env expr in
      let b = value_bool v in
      (Bool (not b), mem')
    | ASSIGNF (expr1, identifier, expr2) ->
      let (r, mem') = eval mem env expr1 in
      let (v, mem'') = eval mem' env expr2 in
      let record = value_record r in
      let l = record identifier in
      (v, Mem.store mem'' l v)
    | FIELD (expr, identifier) ->
      let (v, mem') = eval mem env expr in
      let record = value_record v in
      (Mem.load mem' (record identifier), mem')
    | SEQ (expr1, expr2) ->
      let (v1, mem') = eval mem env expr1 in
      let (v2, mem'') = eval mem' env expr2 in
      (v2, mem'')
    | IF (exp, exp1, exp2) ->
      let (v, mem') = eval mem env exp in
      (
        match v with
        | Bool true -> eval mem' env exp1
        | Bool false -> eval mem' env exp2
        | _ -> raise (Error "TypeError : condition for IF not Bool")
      )
    | WHILE (exp1, exp2) ->
      (
        match (eval mem env exp1) with
        | (Bool true, mem') -> (
          let (v1, mem1) = eval mem' env exp2 in
          let (v2, mem2) = eval mem1 env (WHILE (exp1, exp2)) in
          (v2, mem2)
        )
        | (Bool false, mem') -> (Unit, mem')
        | _ -> raise (Error "TypeError : condition for WHILE not Bool")
      )
    | LETF (funcId, argIdList, expr1, expr2) ->
      let envWithFunc = Env.bind env funcId (Proc (argIdList, expr1, env)) in
      eval mem envWithFunc expr2
    | CALLV (identifier, exprList) ->
      let rec executeParams (_mem, _env, _exprList) = 
        match _exprList with
        | [] -> ([], _mem)
        | exprHead::exprTail -> (
          let (v, mem') = eval _mem _env exprHead in
          let (vList, mem'') = executeParams (mem', _env, exprTail) in
          (v::vList, mem'')
        )
      in
      let rec storeParams (_mem, _valueList) = 
        match _valueList with
        | [] -> ([], _mem)
        | valueHead::valueTail -> (
          let (l, mem') = Mem.alloc _mem in
          let mem'' = Mem.store mem' l valueHead in
          let (locList, mem''') = storeParams (mem'', valueTail) in
          (l::locList, mem''')
        )
      in
      let rec setParamsEnv (_env, _locList, _argIdList) = 
        match _locList, _argIdList with
        | ([], []) -> _env
        | (locHead::locTail, argHead::argTail) -> (
          let env' = Env.bind _env argHead (Addr locHead) in
          setParamsEnv (env', locTail, argTail)
        )
        | _ -> raise (Error "InvalidArg")
      in
      let (valueList, memN) = executeParams (mem, env, exprList) in
      let (locList, memN') = storeParams (memN, valueList) in
      let (argIdList, bodyExpr, procEnv) = lookup_env_proc env identifier in
      let env' = setParamsEnv (procEnv, locList, argIdList) in
      let env'' = Env.bind env' identifier (Proc (argIdList, bodyExpr, procEnv)) in
      eval memN' env'' bodyExpr

    | CALLR (identifier, idList) ->
      let rec setLocEnv (_env', _env, _idList, _argIdList) = 
        match _idList, _argIdList with
        | ([], []) -> _env'
        | (idHead::idTail, argHead::argTail) -> (
          let l = lookup_env_loc _env idHead in
          let env' = Env.bind _env' argHead (Addr l) in
          setLocEnv (env', _env, idTail, argTail)
        )
        | _ -> raise (Error "InvalidArg")
      in
      let (argIdList, bodyExpr, procEnv) = lookup_env_proc env identifier in
      let procEnv' = setLocEnv (procEnv, env, idList, argIdList) in
      let procEnv'' = Env.bind procEnv' identifier (Proc (argIdList, bodyExpr, procEnv)) in
      eval mem procEnv'' bodyExpr

  let run (mem, env, pgm) = 
    let (v, _ ) = eval mem env pgm in
    v
end
