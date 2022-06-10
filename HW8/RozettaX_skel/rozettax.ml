(*
 * SNU 4190.310 Programming Languages 
 * Homework "RozettaX" Skeleton
 *)

let trans_v : Sm5.value -> Sonata.value = function
  | Sm5.Z z  -> Sonata.Z z
  | Sm5.B b  -> Sonata.B b
  | Sm5.L _ -> raise (Sonata.Error "Invalid input program : pushing location")
  | Sm5.Unit -> Sonata.Unit
  | Sm5.R _ -> raise (Sonata.Error "Invalid input program : pushing record")

(* TODO : complete this function *)
let rec trans_obj : Sm5.obj -> Sonata.obj = function
  | Sm5.Val v -> Sonata.Val (trans_v v)
  | Sm5.Id id -> Sonata.Id id
  | Sm5.Fn (arg, command) -> Sonata.Fn(arg, (
    [
      Sonata.BIND "#cont"
    ] @
    (trans' command) @
    [
      Sonata.PUSH (Sonata.Id "#cont"); 
      Sonata.PUSH (Sonata.Val (Z 0)); 
      Sonata.PUSH (Sonata.Id "#nulloc"); 
      Sonata.CALL
    ]
  ))

(* TODO : complete this function *)
and trans' : Sm5.command -> Sonata.command = function
  | Sm5.PUSH obj :: cmds -> Sonata.PUSH (trans_obj obj) :: (trans' cmds)
  | Sm5.POP :: cmds -> Sonata.POP :: (trans' cmds)
  | Sm5.STORE :: cmds -> Sonata.STORE :: (trans' cmds)
  | Sm5.LOAD :: cmds -> Sonata.LOAD :: (trans' cmds)
  | Sm5.JTR (c1, c2) :: cmds ->  Sonata.JTR (
    (trans' c1)@(trans' cmds),
    (trans' c2)@(trans' cmds)
  ) :: []
  | Sm5.MALLOC :: cmds -> Sonata.MALLOC :: (trans' cmds)
  | Sm5.BOX z :: cmds -> Sonata.BOX z :: (trans' cmds)
  | Sm5.UNBOX id :: cmds -> Sonata.UNBOX id :: (trans' cmds)
  | Sm5.BIND id :: cmds -> Sonata.BIND id :: (trans' cmds)
  | Sm5.UNBIND :: cmds -> Sonata.UNBIND :: (trans' cmds)
  | Sm5.GET ::cmds -> Sonata.GET :: (trans' cmds)
  | Sm5.PUT ::cmds -> Sonata.PUT :: (trans' cmds)
  | Sm5.CALL :: cmds -> [
    Sonata.PUSH (Sonata.Fn("#nularg", trans' cmds));
    Sonata.BIND "#cont";
    Sonata.BIND "#fnloc";
    Sonata.MALLOC;
    Sonata.BIND "#fnval";
    Sonata.PUSH (Sonata.Id "#fnval");
    Sonata.STORE;
    Sonata.BIND "#fn";
    Sonata.PUSH (Sonata.Id "#cont");
    Sonata.PUSH (Sonata.Id "#fn");
    Sonata.PUSH (Sonata.Id "#fnval");
    Sonata.LOAD;
    Sonata.PUSH (Sonata.Id "#fnloc");
    Sonata.CALL
  ]
  | Sm5.ADD :: cmds -> Sonata.ADD :: (trans' cmds)
  | Sm5.SUB :: cmds -> Sonata.SUB :: (trans' cmds)
  | Sm5.MUL :: cmds -> Sonata.MUL :: (trans' cmds)
  | Sm5.DIV :: cmds -> Sonata.DIV :: (trans' cmds)
  | Sm5.EQ :: cmds -> Sonata.EQ :: (trans' cmds)
  | Sm5.LESS :: cmds -> Sonata.LESS :: (trans' cmds)
  | Sm5.NOT :: cmds -> Sonata.NOT :: (trans' cmds)
  | [] -> []

(* TODO : complete this function *)
let trans : Sm5.command -> Sonata.command = fun command ->
  [Sonata.MALLOC; Sonata.BIND "#nulloc"] @ trans' command


(*

sm5 call
    (l::v::(x,C',E')::S,  M,          E,  call::C,        K)
 => (S,             M{l->v},  (x,l)::E',       C', (C,E)::K)

as sonata
    (l::v::(x,C',E')::S, M, E, call::C)
=>  (S, M{l->v}, (x,l)::E', C'::(call fn(_,C,E)))

as sonata: step by step
    (l::v::(x,C'::(CALL #cont),E')::S, M, E, C)
 => ((_,C,E)::l::v::(x,C'::(CALL #cont),E')::S, M, E, C) by PUSH (_, C)
 => (l::v::(x,C'::(CALL #cont),E')::S, M, (#cont,(_,C,E))::E, C) by BIND #cont
 => (v::(x,C'::(CALL #cont),E')::S, M, (#fnloc,l)::(#cont,(_,C,E))::E, C) by BIND #fnloc
 => (l'::v::(x,C'::(CALL #cont),E')::S, M, (#fnloc,l)::(#cont,(_,C,E))::E, C) by MALLOC
 => (v::(x,C'::(CALL #cont),E')::S, M, (#fnval,l')::(#fnloc,l)::(#cont,(_,C,E))::E, C) by BIND #fnval
 => (l'::v::(x,C'::(CALL #cont),E')::S, M, (#fnval,l')::(#fnloc,l)::(#cont,(_,C,E))::E, C) by PUSH #fnval
 => ((x,C'::(CALL #cont),E')::S, M{l'->v}, (#fnval,l')::(#fnloc,l)::(#cont,(_,C,E))::E, C) by STORE
 => (S, M{l'->v}, (#fn,(x,C'::(CALL #cont),E'))::(#fnval,l')::(#fnloc,l)::(#cont,(_,C,E))::E, C) by BIND #fn
 => ((#cont,(_,C,E))::S, M{l'->v}, (#fn,(x,C'::(CALL #cont),E'))::(#fnval,l')::(#fnloc,l)::(#cont,(_,C,E))::E, C) by PUSH #cont
 => ((x,C'::(CALL #cont),E')::(#cont,(_,C,E))::S, M{l'->v}, (#fn,(x,C'::(CALL #cont),E'))::(#fnval,l')::(#fnloc,l)::(#cont,(_,C,E))::E, C) by PUSH #fn
 => (l'::(x,C'::(CALL #cont),E')::(#cont,(_,C,E))::S, M{l'->v}, (#fn,(x,C'::(CALL #cont),E'))::(#fnval,l')::(#fnloc,l)::(#cont,(_,C,E))::E, C) by PUSH #fnval
 => (v::(x,C'::(CALL #cont),E')::(#cont,(_,C,E))::S, M{l'->v}, (#fn,(x,C'::(CALL #cont),E'))::(#fnval,l')::(#fnloc,l)::(#cont,(_,C,E))::E, C) by LOAD
 => (l::v::(x,C'::(CALL #cont),E')::(#cont,(_,C,E))::S, M{l'->v}, (#fn,(x,C'::(CALL #cont),E'))::(#fnval,l')::(#fnloc,l)::(#cont,(_,C,E))::E, C) by PUSH #fnloc
 => ((_,C,E)::S, M{l'->v}{l->v}, (x,l)::E', C'::(CALL #cont)) by CALL
 => (S, M{l'->v}{l->v}, (#cont,(_,C,E))::(x,l)::E', C'::(CALL #cont)) by BIND #cont
 => (S', M', ()::(#cont,(_,C,E))::(x,l)::E', (CALL #cont)) by executing C'
 => ((_,C,E)::S', M', ()::(#cont,(_,C,E))::(x,l)::E', (CALL #cont)) by PUSH #cont
 => (0::(_,C,E)::S', M', ()::(#cont,(_,C,E))::(x,l)::E', (CALL #cont)) by PUSH 0
 => (#nulloc::0::(_,C,E)::S', M', ()::(#cont,(_,C,E))::(x,l)::E', (CALL #cont)) by PUSH #nulloc [E' has #nulloc from beginning]
 => (S', M'{#nulloc->0}, (_,#nulloc)::E, C) by CALL

so fn in sonata should be:
  BIND #cont (believeing that stack top is continuation func)
  execute C' (trans' cmds)
  PUSH #cont
  PUSH 0
  PUSH #nulloc (E in genesis has #nulloc)
  CALL (machine restores E, C)

and translated CALL of sm5 (to sonata) should be:
  PUSH (_, C)
  BIND #cont
  BIND #fnloc
  BIND #fnval
  BIND #fn  (now stack is S)
  PUSH #cont
  PUSH #fn
  PUSH #fnval
  PUSH #fnloc (now stack is l::v::#fn::#cont::S)
  CALL

in the beginning...
  (S0, M0, E0, empty)
  (l::S0, M0, E0, empty) by MALLOC
  (S0, M0, (#nulloc, l)::E0, empty) by BIND #nulloc
  (S0, M0, (#nulloc, l)::E0, C) by trans' C

  

 *)