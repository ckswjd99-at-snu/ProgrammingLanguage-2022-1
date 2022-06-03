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
  | TUnbound of var

exception CommonError of string

type typeEquation = 
  | Single of typ * typ
  | Multi of typ * typ list

type typeEnv = var -> typ

let emptySystem = []
let emptyTypeEnv : typeEnv = fun x -> TUnbound x
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


let rec typToString (expr : typ) = (
  match expr with
  | TInt -> "TInt"
  | TBool -> "TBool"
  | TString -> "TString"
  | TPair (typ1, typ2) -> "TPair (" ^ (typToString typ1) ^ ", " ^ (typToString typ2) ^ ")"
  | TLoc typ1 -> "TLoc (" ^ (typToString typ1) ^ ")"
  | TFun (typ1, typ2) -> "TFun (" ^ (typToString typ1) ^ " -> " ^ (typToString typ2) ^ ")"
  | TVar var -> "TVar " ^ var
  | TUnbound tub -> "TUnbound " ^ tub
) 
let typListToString (typList : typ list) = (
  let rec tltsRec (tl : typ list) = (
    match tl with
    | tlHead::tlTail -> "; " ^ (typToString tlHead) ^ (tltsRec tlTail)
    | _ -> ""
  ) in
  "[" ^ (tltsRec typList) ^ "]"
)

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
  | TUnbound _ -> typExpr

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

let equationToString (equation : typeEquation) : string = 
  match equation with
  | Single (fromTyp, toTyp) -> (
    "Eq: " ^ (typToString fromTyp) ^ " = " ^ (typToString toTyp)
  )
  | Multi (fromTyp, toTypList) -> (
    "Eq: " ^ (typToString fromTyp) ^ " = " ^ (typListToString toTypList)
  )
  

let rec reduceTyp (env : typeEnv) (inputTyp : typ) : typ =
  match inputTyp with
  | TInt | TBool | TString -> inputTyp
  | TPair (tp1, tp2) -> TPair (reduceTyp env tp1, reduceTyp env tp2)
  | TLoc tl -> TLoc (reduceTyp env tl)
  | TFun (tf1, tf2) -> TFun (reduceTyp env tf1, reduceTyp env tf2)
  | TVar tv -> (
    match env tv with
    | TUnbound _ -> TVar tv
    | _ -> reduceTyp env (env tv)
  )
  | _ -> raise (CommonError "unbound during reduceTyp!!")

let rec solveSingleEquationToEnv (env : typeEnv) (equation : typeEquation) : typeEnv = 
  let reducedEquation = (
    match equation with
    | Single (leftTyp, rightTyp) -> Single (reduceTyp env leftTyp, reduceTyp env rightTyp)
    | Multi _ -> raise (CommonError "multi equation for solveSingleEq!!")
  ) in
  (* let _ = print_string (equationToString reducedEquation) in *)
  (* let _ = print_string "\n" in *)
  match reducedEquation with
  | Single (TVar vt, toTyp) -> env @+ (vt, toTyp)
  | Single (fromTyp, TVar vt) -> env @+ (vt, fromTyp)
  | Single (TPair (tp11, tp12), TPair (tp21, tp22)) -> (
    let equation1 = (Single (tp11, tp21)) in
    let env' = solveSingleEquationToEnv env equation1 in
    let equation2 = (Single (tp12, tp22)) in
    let env'' = solveSingleEquationToEnv env' equation2 in
    env''
  )
  | Single (TLoc tl1, TLoc tl2) -> (
    let reducedEquation = (Single (tl1, tl2)) in
    solveSingleEquationToEnv env reducedEquation
  )
  | Single (TFun (tf11, tf12), TFun (tf21, tf22)) -> (
    let equation1 = (Single (tf11, tf21)) in
    let env' = solveSingleEquationToEnv env equation1 in
    let equation2 = (Single (tf12, tf22)) in
    let env'' = solveSingleEquationToEnv env' equation2 in
    env''
  )
  | Single (a, b) -> (
    if type_is_same (a, b) then
      env
    else
      raise (M.TypeError "cannot match type!!")
  )
  | Multi (fromTyp, toTypList) -> raise (CommonError "cannot match multi type!!")

let rec create_type_equation (mEnv: typeEnv) (tEnv: typeEnv) (exp: M.exp) (exp_typ: typ) : typeEnv * typeEquation list =
  match exp with
  | CONST (S s) -> (
    let currentEquation = Single (exp_typ, TString) in
    let tEnv' = solveSingleEquationToEnv tEnv currentEquation in
    (tEnv', [])
  )
  | CONST (N n) -> (
    let currentEquation = Single (exp_typ, TInt) in
    let tEnv' = solveSingleEquationToEnv tEnv currentEquation in
    (tEnv', [])
  )
  | CONST (B b) -> (
    let currentEquation = Single (exp_typ, TBool) in
    let tEnv' = solveSingleEquationToEnv tEnv currentEquation in
    (tEnv', [])
  )
  | VAR x -> (
    let currentEquation = Single (exp_typ, mEnv x) in
    let tEnv' = solveSingleEquationToEnv tEnv currentEquation in
    (tEnv', [])
  )
  | FN (x, e) -> (
    let tv1 = TVar (new_var ()) in
    let tv2 = TVar (new_var ()) in
    let mEnv' = mEnv @+ (x, tv1) in
    let currentEquation = Single (exp_typ, TFun (tv1, tv2)) in
    let tEnv' = solveSingleEquationToEnv tEnv currentEquation in
    create_type_equation mEnv' tEnv' e tv2
  )
  | APP (e1, e2) -> (
    let tv2 = TVar (new_var ()) in
    let tv1 = TFun (tv2, exp_typ) in
    let (tEnv', meqList1) = create_type_equation mEnv tEnv e1 tv1 in
    let (tEnv'', meqList2) = create_type_equation mEnv tEnv' e2 tv2 in
    (tEnv'', meqList1 @ meqList2)
  )
  | IF (e1, e2, e3) -> (
    let tv2 = TVar (new_var ()) in
    let (tEnv', meqList1) = create_type_equation mEnv tEnv e1 TBool in
    let (tEnv'', meqList2) = create_type_equation mEnv tEnv' e2 tv2 in
    let (tEnv''', meqList3) = create_type_equation mEnv tEnv'' e3 tv2 in
    let currentEq1 = Single (exp_typ, tv2) in
    let tEnv'''' = solveSingleEquationToEnv tEnv''' currentEq1 in
    (tEnv'''', meqList1 @ meqList2 @ meqList3)
  )
  | BOP (op, e1, e2) -> (
    match op with
    | ADD | SUB -> (
      let (tEnv', meqList1) = create_type_equation mEnv tEnv e1 TInt in
      let (tEnv'', meqList2) = create_type_equation mEnv tEnv' e2 TInt in
      let currentEq1 = Single (exp_typ, TInt) in
      let tEnv''' = solveSingleEquationToEnv tEnv'' currentEq1 in
      (tEnv''', meqList1 @ meqList2)
    )
    | AND | OR -> (
      let (tEnv', meqList1) = create_type_equation mEnv tEnv e1 TBool in
      let (tEnv'', meqList2) = create_type_equation mEnv tEnv' e2 TBool in
      let currentEq1 = Single (exp_typ, TBool) in
      let tEnv''' = solveSingleEquationToEnv tEnv'' currentEq1 in
      (tEnv''', meqList1 @ meqList2)
    )
    | EQ -> (
      let tv = TVar (new_var ()) in
      let tvl = TVar (new_var ()) in
      let (tEnv', meqList1) = create_type_equation mEnv tEnv e1 tv in
      let (tEnv'', meqList2) = create_type_equation mEnv tEnv' e2 tv in
      let currentEq1 = Single (exp_typ, TBool) in
      let tEnv''' = solveSingleEquationToEnv tEnv'' currentEq1 in
      (tEnv''', [Multi (tv, [TInt; TBool; TString; TLoc tvl])] @ meqList1 @ meqList2)
    )
  )
  | READ -> (
    let currentEquation = Single (exp_typ, TInt) in
    let tEnv' = solveSingleEquationToEnv tEnv currentEquation in
    (tEnv', [])
  )
  | WRITE e -> (
    let tv = TVar (new_var ()) in
    let (tEnv', meqList1) = create_type_equation mEnv tEnv e tv in
    let currentEq1 = Single (exp_typ, tv) in
    let tEnv'' = solveSingleEquationToEnv tEnv' currentEq1 in
    (tEnv'', [Multi (tv, [TInt; TBool; TString])] @ meqList1)
  )
  | PAIR (e1, e2) -> (
    let tv1 = TVar (new_var ()) in
    let tv2 = TVar (new_var ()) in
    let (tEnv', meqList1) = create_type_equation mEnv tEnv e1 tv1 in
    let (tEnv'', meqList2) = create_type_equation mEnv tEnv' e2 tv2 in
    let currentEq1 = Single (exp_typ, TPair (tv1, tv2)) in
    let tEnv''' = solveSingleEquationToEnv tEnv'' currentEq1 in
    (tEnv''', meqList1 @ meqList2)
  )
  | FST e -> (
    let tv1 = TVar (new_var ()) in
    let tv2 = TVar (new_var ()) in
    let (tEnv', meqList1) = create_type_equation mEnv tEnv e (TPair (tv1, tv2)) in
    let currentEq1 = Single (exp_typ, tv1) in
    let tEnv'' = solveSingleEquationToEnv tEnv' currentEq1 in
    (tEnv'', meqList1)
  )
  | SND e -> (
    let tv1 = TVar (new_var ()) in
    let tv2 = TVar (new_var ()) in
    let (tEnv', meqList1) = create_type_equation mEnv tEnv e (TPair (tv1, tv2)) in
    let currentEq1 = Single (exp_typ, tv2) in
    let tEnv'' = solveSingleEquationToEnv tEnv' currentEq1 in
    (tEnv'', meqList1)
  )
  | LET (dec, e2) -> (
    match dec with
    | VAL (id, e1) -> (
      let tv1 = TVar (new_var ()) in
      let (tEnv', meqList1) = create_type_equation mEnv tEnv e1 tv1 in
      let (tEnv'', meqList2) = create_type_equation (mEnv @+ (id, tv1)) tEnv' e2 exp_typ in
      (tEnv'', meqList1 @ meqList2)
    )
    | REC (id1, id2, e1) -> (
      let tve = TVar (new_var ()) in
      let tva = TVar (new_var ()) in
      let (tEnv', meqList1) = create_type_equation ((mEnv @+ (id1, TFun (tva, tve))) @+ (id2, tva)) tEnv e1 tve in
      let (tEnv'', meqList2) = create_type_equation (mEnv @+ (id1, TFun (tva, tve))) tEnv' e2 exp_typ in
      (tEnv'', meqList1 @ meqList2)
    )
  )
  | MALLOC e -> (
    let tv = TVar (new_var ()) in
    let (tEnv', meqList1) = create_type_equation mEnv tEnv e tv in
    let currentEq1 = Single (exp_typ, TLoc tv) in
    let tEnv'' = solveSingleEquationToEnv tEnv' currentEq1 in
    (tEnv'', meqList1)
  )
  | ASSIGN (e1, e2) -> (
    let (tEnv', meqList1) = create_type_equation mEnv tEnv e1 (TLoc (exp_typ)) in
    let (tEnv'', meqList2) = create_type_equation mEnv tEnv' e2 exp_typ in
    (tEnv'', meqList1 @ meqList2)
  )
  | BANG e -> (
    create_type_equation mEnv tEnv e (TLoc (exp_typ))
  )
  | SEQ (e1, e2) -> (
    let tv1 = TVar (new_var ()) in
    let (tEnv', meqList1) = create_type_equation mEnv tEnv e1 tv1 in
    let (tEnv'', meqList2) = create_type_equation mEnv tEnv' e2 exp_typ in
    (tEnv'', meqList1 @ meqList2)
  )

let checkMultiEquation env meq =
  match meq with
  | Multi (leftTyp, rightTypList) -> (
    let reducedLeftTyp = reduceTyp env leftTyp in
    let reducedRightTypList = List.map (fun x -> reduceTyp env x) rightTypList in
    List.exists (fun x -> type_is_same (x, reducedLeftTyp)) reducedRightTypList
  )
  | Single _ -> raise (CommonError "single eq for checkMultiEq!!")


(* TODO : Implement this function *)
let check : M.exp -> M.types = fun exp -> (
  let tvname = new_var () in
  let tv = TVar (tvname) in
  let (tEnv, meqList) = create_type_equation emptyTypeEnv emptyTypeEnv exp tv in
  let meqValidList = List.map (fun x -> checkMultiEquation tEnv x) meqList in
  let meqValid = not (List.mem false meqValidList) in
  if meqValid then
    typToType (typToTerminal tEnv (tEnv tvname))
  else
    raise (M.TypeError "cannot match type!!")
)

