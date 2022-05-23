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
exception TypeError of string

type typeEquation = typ * typ list
type typeEquationSystem = typeEquation list

type simplifiedTypeEq = 
  | Single of typ * typ
  | Multi of typ * typ list
type simplifiedTypeEqSystem = simplifiedTypeEq list

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

let rec typToString (expr : typ) = (
  match expr with
  | TInt -> "TInt"
  | TBool -> "TBool"
  | TString -> "TString"
  | TPair (typ1, typ2) -> "TPair (" ^ (typToString typ1) ^ ", " ^ (typToString typ2) ^ ")"
  | TLoc typ1 -> "TLoc (" ^ (typToString typ1) ^ ")"
  | TFun (typ1, typ2) -> "TFun (" ^ (typToString typ1) ^ " -> " ^ (typToString typ2) ^ ")"
  | TVar var -> "TVar " ^ var
) 
let typListToString (typList : typ list) = (
  let rec tltsRec (tl : typ list) = (
    match tl with
    | tlHead::tlTail -> "; " ^ (typToString tlHead) ^ (tltsRec tlTail)
    | _ -> ""
  ) in
  "[" ^ (tltsRec typList) ^ "]"
)
let printSys (system: simplifiedTypeEqSystem) =
  let rec typeEquationToString (eq : simplifiedTypeEq) = ( 
    match eq with
    | Single (fromTyp, toTyp) -> "Eq : " ^ (typToString fromTyp) ^ " = " ^ (typToString toTyp) ^ "\n"
    | Multi (fromTyp, toTypList) -> "Eq : " ^ (typToString fromTyp) ^ " = " ^ (typListToString toTypList) ^ "\n"
  ) in
  List.iter (fun x -> print_string (typeEquationToString x)) system


let rec removeUseless (system : simplifiedTypeEqSystem) : simplifiedTypeEqSystem =
  match system with
  | sysHead::sysTail -> (
    match sysHead with
    | Single (fromTyp, toTyp) -> if type_is_same (fromTyp, toTyp) then removeUseless sysTail else sysHead::(removeUseless sysTail)
    | Multi (fromTyp, toTypList) -> sysHead::(removeUseless sysTail)
  )
  | [] -> []

let rec solve (system : simplifiedTypeEqSystem) (env : typeEnv) : simplifiedTypeEqSystem * typeEnv =
  let rec replaceVar (originalTyp : typ) ((fromVar : typ), (toTyp : typ)) : typ = 
    match originalTyp with
    | TInt | TBool | TString -> originalTyp
    | TPair (tp1, tp2) -> TPair (replaceVar tp1 (fromVar, toTyp), replaceVar tp2 (fromVar, toTyp))
    | TLoc tl -> TLoc (replaceVar tl (fromVar, toTyp))
    | TFun (tf1, tf2) -> TFun (replaceVar tf1 (fromVar, toTyp), replaceVar tf2 (fromVar, toTyp))
    | TVar vt -> if (type_is_same (originalTyp, fromVar)) then toTyp else originalTyp
  in
  let rec replaceVarInSys (system : simplifiedTypeEqSystem) ((fromVar : typ), (toTyp : typ)) : simplifiedTypeEqSystem = 
    match system with
    | (Single (leftTyp, rightTyp))::sysTail -> (
      (Single ((replaceVar leftTyp (fromVar, toTyp)), (replaceVar rightTyp (fromVar, toTyp))))::(replaceVarInSys sysTail (fromVar, toTyp))
    )
    | (Multi (leftTyp, rightTypList))::sysTail -> (
      (Multi ((replaceVar leftTyp (fromVar, toTyp)), List.map (fun x -> replaceVar x (fromVar, toTyp)) rightTypList))::(replaceVarInSys sysTail (fromVar, toTyp))
    )
    | [] -> system
  in
  match system with
  | sysHead::sysTail -> (
    match sysHead with
    | Single (TVar vt, toTyp) -> (
      let env' = env @+ (vt, toTyp) in
      let _ = print_string ("Replacement: TVar " ^ vt ^ " -> " ^ (typToString toTyp) ^ "\n") in
      let system' = replaceVarInSys sysTail (TVar vt, toTyp) in
      solve system' env'
    )
    | Single (fromTyp, TVar vt) -> solve ((Single (TVar vt, fromTyp))::sysTail) env
    | Single (TPair (tp11, tp12), TPair (tp21, tp22)) -> (
      let resolvedSys = (Single (tp11, tp21))::(Single (tp12, tp22))::sysTail in
      solve resolvedSys env
    )
    | Single (TLoc tl1, TLoc tl2) -> (
      let resolvedSys = (Single (tl1, tl2))::sysTail in
      solve resolvedSys env
    )
    | Single (TFun (tf11, tf12), TFun (tf21, tf22)) -> (
      let resolvedSys = (Single (tf11, tf21))::(Single (tf12, tf22))::sysTail in
      solve resolvedSys env
    )
    | Single (a, b) -> (
      if type_is_same (a, b) then
        solve sysTail env
      else
        raise (TypeError "cannot match type!!")
    )
    | Multi (fromTyp, toTypList) -> (
      let sameTypeList = List.filter (fun x -> type_is_same (fromTyp, x)) toTypList in
      let isValid = (List.length sameTypeList) > 0 in
      if isValid then
        solve sysTail env
      else
        raise (TypeError "cannot match multi type!!")
    )
  )
  | [] -> (system, env)

let rec isTerminal (typExpr : typ) : bool =
  match typExpr with
  | TInt | TBool | TString -> true
  | TPair (tp1, tp2) -> (isTerminal tp1) && (isTerminal tp2)
  | TLoc tl -> isTerminal tl
  | TFun (tf1, tf2) -> (isTerminal tf1) && (isTerminal tf2)
  | TVar _ -> false

let rec typToTerminal (env : typeEnv) (typExpr : typ) =
  match typExpr with
  | TInt | TBool | TString -> typExpr
  | TPair (tp1, tp2) -> TPair ((typToTerminal env tp1), (typToTerminal env tp2))
  | TLoc tl -> TLoc (typToTerminal env tl)
  | TFun (tf1, tf2) -> TFun ((typToTerminal env tf1), (typToTerminal env tf2))
  | TVar tva -> typToTerminal env (env tva)

let rec typToType (input : typ) : M.types = (
  match input with
  | TInt -> M.TyInt
  | TBool -> M.TyBool
  | TString -> M.TyString
  | TPair (tp1, tp2) -> TyPair (typToType tp1, typToType tp2)
  | TLoc tl -> TyLoc (typToType tl)
  | TFun (tf1, tf2) -> TyArrow (typToType tf1, typToType tf2)
  | TVar tv -> raise (CommonError "typ has var!!")
)

(* TODO : Implement this function *)
let check : M.exp -> M.types = fun exp -> (
  let tv = TVar (new_var ()) in
  let rawSystem = create_type_equation emptyTypeEnv [] exp tv in
  let singleSystem = List.map (function (fromTyp, toTyp::[]) -> Single (fromTyp, toTyp)) (List.filter (function (_, x) -> (List.length x) = 1) rawSystem) in
  let multiSystem = List.map (function (fromTyp, toTypList) -> Multi (fromTyp, toTypList)) (List.filter (function (_, x) -> (List.length x) > 1) rawSystem) in
  let system = singleSystem @ multiSystem in
  let _ = Printf.printf "#eq: %d\n" (List.length system) in
  (* let _ = printSys system in *)
  let (_, env) = solve system emptyTypeEnv in
  typToType (typToTerminal env tv)
)

