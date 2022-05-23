(*
 * SNU 4190.310 Programming Languages
 * Type Checker Skeleton Code
 *)

open M
open Pp

type var = string

let count = ref 0 

let new_var () = 
  let _ = count := !count +1 in
  "x_" ^ (string_of_int !count)

type typ = 
  | TInt
  | TBool
  | TString
  | TPair of typ * typ
  | TLoc of typ
  | TFun of typ * typ
  | TVar of var
  (* Modify, or add more if needed *)

exception CommonError of string

type typeEquation = typ * typ list
type typeEquationSystem = typeEquation list

type typeEnv = var -> typ

let emptySystem = []
let emptyTypeEnv : typeEnv = fun x -> raise (CommonError ("unbound type env: " ^ x))
let (@+) f (x, v) = (fun y -> if y = x then v else f y)

let rec type_is_same ((t1 : typ), (t2 : typ)) : bool =
  match t1, t2 with
  | (TInt, TInt)
  | (TBool, TBool)
  | (TString, TString) -> true
  | (TPair (tp11, tp12), TPair (tp21, tp22)) -> (type_is_same (tp11, tp21)) && (type_is_same (tp12, tp22))
  | (TLoc tl1, TLoc tl2) -> type_is_same (tl1, tl2)
  | (TFun (tf11, tf12), TFun (tf21, tf22)) -> (type_is_same (tf11, tf21)) && (type_is_same (tf12, tf22))
  | (TVar var1, TVar var2) -> var1 = var2
  | _ -> false

let rec create_type_equation (env: typeEnv) (sys: typeEquationSystem) (exp: M.exp) (exp_typ: typ) : typeEquationSystem =
  match exp with
  | CONST (S s) -> (exp_typ, [TString])::sys
  | CONST (N n) -> (exp_typ, [TInt])::sys
  | CONST (B b) -> (exp_typ, [TBool])::sys
  | VAR x -> (exp_typ, [env x])::sys
  | FN (x, e) -> (
    let tv1 = TVar (new_var ()) in
    let tv2 = TVar (new_var ()) in
    (exp_typ, [TFun (tv1, tv2)])::(create_type_equation (env @+ (x, tv1)) sys e tv2) @ sys
  )
  | APP (e1, e2) -> (
    let tv2 = TVar (new_var ()) in
    let tv1 = TFun (tv2, exp_typ) in
    let sys' = create_type_equation env sys e1 tv1 in
    let sys'' = create_type_equation env sys' e2 tv2 in
    sys''
  )
  | IF (e1, e2, e3) -> (
    let tv2 = TVar (new_var ()) in
    let sys' = create_type_equation env sys e1 TBool in
    let sys'' = create_type_equation env sys' e2 tv2 in
    let sys''' = create_type_equation env sys'' e3 tv2 in
    (exp_typ, [tv2])::sys'''
  )
  | BOP (op, e1, e2) -> (
    match op with
    | ADD | SUB -> (
      let sys' = create_type_equation env sys e1 TInt in
      let sys'' = create_type_equation env sys' e2 TInt in
      (exp_typ, [TInt])::sys''
    )
    | AND | OR -> (
      let sys' = create_type_equation env sys e1 TBool in
      let sys'' = create_type_equation env sys' e2 TBool in
      (exp_typ, [TBool])::sys''
    )
    | EQ -> (
      let tv = TVar (new_var ()) in
      let tvl = TVar (new_var ()) in
      let sys' = create_type_equation env sys e1 tv in
      let sys'' = create_type_equation env sys' e2 tv in
      (exp_typ, [TBool])::(tv, [TInt; TBool; TString; TLoc tvl])::sys''
    )
  )
  | READ -> (exp_typ, [TInt])::sys
  | WRITE e -> (
    let tv = TVar (new_var ()) in
    let sys' = create_type_equation env sys e tv in
    (tv, [TInt; TBool; TString])::sys'
  )
  | PAIR (e1, e2) -> (
    let tv1 = TVar (new_var ()) in
    let tv2 = TVar (new_var ()) in
    let sys' = create_type_equation env sys e1 tv1 in
    let sys'' = create_type_equation env sys' e2 tv2 in
    (exp_typ, [TPair (tv1, tv2)])::sys''
  )
  | FST e -> (
    let tv1 = TVar (new_var ()) in
    let tv2 = TVar (new_var ()) in
    let sys' = create_type_equation env sys e (TPair (tv1, tv2)) in
    (exp_typ, [tv1])::sys'
  )
  | SND e -> (
    let tv1 = TVar (new_var ()) in
    let tv2 = TVar (new_var ()) in
    let sys' = create_type_equation env sys e (TPair (tv1, tv2)) in
    (exp_typ, [tv2])::sys'
  )
  | LET (dec, e2) -> (
    match dec with
    | VAL (id, e1) -> (
      let tv1 = TVar (new_var ()) in
      let sys' = create_type_equation env sys e1 tv1 in
      let sys'' = create_type_equation (env @+ (id, tv1)) sys' e2 exp_typ in
      sys''
    )
    | REC (id1, id2, e1) -> (
      let tve = TVar (new_var ()) in
      let tva = TVar (new_var ()) in
      let sys' = create_type_equation ((env @+ (id1, TFun (tva, tve))) @+ (id2, tva)) sys e1 tve in
      let sys'' = create_type_equation (env @+ (id1, TFun (tva, tve))) sys' e2 exp_typ in
      sys''
    )
  )
  | MALLOC e -> (
    let tv = TVar (new_var ()) in
    let sys' = create_type_equation env sys e tv in
    (exp_typ, [TLoc tv])::sys'
  )
  | ASSIGN (e1, e2) -> (
    let sys' = create_type_equation env sys e1 (TLoc (exp_typ)) in
    let sys'' = create_type_equation env sys' e2 exp_typ in
    sys''
  )
  | BANG e -> (
    create_type_equation env sys e (TLoc (exp_typ))
  )
  | SEQ (e1, e2) -> (
    let tv1 = TVar (new_var ()) in
    let sys' = create_type_equation env sys e1 tv1 in
    let sys'' = create_type_equation env sys' e2 exp_typ in
    sys''
  )

let printSys (system: typeEquationSystem) =
  let rec typToString (expr : typ) = (
    match expr with
    | TInt -> "TInt"
    | TBool -> "TBool"
    | TString -> "TString"
    | TPair (typ1, typ2) -> "TPair (" ^ (typToString typ1) ^ ", " ^ (typToString typ2) ^ ")"
    | TLoc typ1 -> "TLoc " ^ (typToString typ1)
    | TFun (typ1, typ2) -> "TFun (" ^ (typToString typ1) ^ " -> " ^ (typToString typ2) ^ ")"
    | TVar var -> "TVar " ^ var
  ) 
  and typListToString (typList : typ list) = (
    let rec tltsRec (tl : typ list) = (
      match tl with
      | tlHead::tlTail -> "; " ^ (typToString tlHead) ^ (tltsRec tlTail)
      | _ -> ""
    ) in
    "[" ^ (tltsRec typList) ^ "]"
  ) in
  let rec typeEquationToString (eq : typeEquation) = (
    match eq with
    | (typ1, typlist) -> (
      "Eq: " ^ (typToString typ1) ^ " = " ^ (typListToString typlist) ^ "\n"
    )
  ) in
  List.iter (fun x -> print_string (typeEquationToString x)) system

let isSingleEquation (equation : typeEquation) : bool =
  match equation with
  | (_, typlist) -> (List.length typlist) = 1

let rec removeUseless (system : typeEquationSystem) : typeEquationSystem =
  match system with
  | sysHead::sysTail -> (
    if isSingleEquation sysHead then
      let leftOfEq = fst sysHead in
      let rightOfEq = List.hd (snd sysHead) in
      if type_is_same (leftOfEq, rightOfEq) then
        removeUseless sysTail
      else
        sysHead::(removeUseless sysTail)
    else
      sysHead::(removeUseless sysTail)
  )
  | [] -> []

let rec replaceVar (equation : typeEquation) (system : typeEquationSystem) =
  let rec replaceVarInEquation (targetEq : typeEquation) (fromTyp : typ) (toTyp : typ) : typeEquation = (
    match targetEq with
    | (leftTyp, rightList) -> (
      (
        (if type_is_same (leftTyp, fromTyp) then toTyp else leftTyp),
        List.map (fun x -> if type_is_same (x, fromTyp) then toTyp else leftTyp) rightList
      )
    )
  ) in
  match equation with
  | (TVar tv, rt::[]) -> List.map (fun eqInSys -> replaceVarInEquation eqInSys (TVar tv) rt) system
  | (lt, (TVar tv)::[]) -> List.map (fun eqInSys -> replaceVarInEquation eqInSys (TVar tv) lt) system
  | (TFun (tf11, tf12), (TFun (tf21, tf22))::[]) -> (
    let system' = List.map (fun eqInSys -> replaceVarInEquation eqInSys tf11 tf21) system in
    let system'' = List.map (fun eqInSys -> replaceVarInEquation eqInSys tf21 tf22) system' in
    system''
  )
  | (TPair (tp11, tp12), (TPair (tp21, tp22))::[]) -> (
    let system' = List.map (fun eqInSys -> replaceVarInEquation eqInSys tp11 tp21) system in
    let system'' = List.map (fun eqInSys -> replaceVarInEquation eqInSys tp21 tp22) system' in
    system''
  )
  | (TLoc tl1, (TLoc tl2)::[]) -> List.map (fun eqInSys -> replaceVarInEquation eqInSys tl1 tl2) system
  | (a, b::[]) -> (
    if type_is_same (a, b) then system else raise (M.TypeError "There is no matched type")
  )
  | _ -> raise (CommonError "equation for replacement should be single equation")


let replaceVarInSys (system : typeEquationSystem) =
  let rec rvisRec system num =
    if List.length system = num then
      system
    else
      let nowEq = List.nth system num in
      if isSingleEquation nowEq then (
        let system' = replaceVar nowEq system in
        rvisRec system' (num+1)
      )
      else
        rvisRec system (num+1)
  in
  rvisRec system 0

(* TODO : Implement this function *)
let check : M.exp -> M.types = fun exp -> (
  let tv = TVar (new_var ()) in
  let rawSystem = create_type_equation emptyTypeEnv [] exp tv in
  let singleSystem = List.filter (function (_, x) -> (List.length x) = 1) rawSystem in
  let multiSystem = List.filter (function (_, x) -> (List.length x) > 1) rawSystem in
  let system = singleSystem @ multiSystem in
  let _ = Printf.printf "#eq: %d\n" (List.length system) in
  let _ = printSys system in
  let system' = removeUseless system in
  let _ = Printf.printf "#reduced: %d\n" (List.length system') in
  let _ = printSys system' in
  let system'' = replaceVarInSys system' in
  let _ = Printf.printf "#reduced: %d\n" (List.length system'') in
  let _ = printSys system'' in

  M.TyBool
)

