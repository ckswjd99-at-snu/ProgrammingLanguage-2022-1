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
and typenv = var -> typ 
and subst = typ -> typ
 (* Modify, or add more if needed *)

type constraints = Unique of typ * typ
				 | Or of typ * typ list
and typeEquation = constraints list

(*
   타입 constraints : 연립방정식에서, 각 방정식을 표현하는 타입.
   ex) Unique t1 t2 -> t1=t2 라는 방정식
	   Unique t1 TBool -> t1=TBool 이라는 방정식
       Or t1 [t2;t3;t4] -> t1=t2 or t1=t3 or t1=t4라는 방정식
	   Or t1 [TBool;TInt;TString] -> t1=TBool or t1=TInt or t1=TString 이라는 방정식
   타입 typeequ : constraints 들의 리스트. 각 타입방정식이 연립되어있음을 뜻함.
   ex) c1,c2가 constraints이고 (c1 : t1=t2, c2 : t2=TBool) c1::c2::[] 가 typeequ이면,
		이는 연립방정식 t1=t2, t2=TBool 을 나타냄. 따라서  이 방정식의 해는  t1=t2=TBool.
*) 

let (@+) f (x, v) = (fun y -> if y = x then v else f y)
  (* (env' @+ (x, v2)) *)
let emptyEnv = (fun x -> raise (M.TypeError ("unbound type: " ^ x)))
let (@@) s1 s2 = (fun t -> s1 (s2 t)) (* substitution composition *)

let rec hasSameType (a:typ) (b:typ) : bool = 
	match (a,b) with
	| (TInt,TInt) -> true
	| (TBool,TBool) -> true
	| (TString,TString) -> true
	| (TLoc ta,TLoc tb) -> (hasSameType ta tb)
	| (TPair (a1,a2),TPair (b1,b2)) -> ((hasSameType a1 b1) && (hasSameType a2 b2))
	| (TFun (a1,a2),TFun (b1,b2)) -> ((hasSameType a1 b1) && (hasSameType a2 b2))
	| (TVar sa,TVar sb) -> (if ((String.compare sa sb)==0) then true else false)
	| _ -> false 	

let emptysub : subst = (fun x->x)

let substitution (x : var) (t : typ) : subst = 
	let rec sub (inputyp : typ) : typ =
		match inputyp with
		| TInt -> TInt
		| TBool -> TBool
		| TString -> TString
		| TPair (a,b) -> TPair (sub a,sub b)
		| TLoc l -> TLoc (sub l)
		| TFun (a,b) -> TFun (sub a, sub b)
		| TVar v -> if ((String.compare v x)==0) then t else inputyp
	in sub

	
let rec unify (ty1 : typ) (ty2 : typ) : subst =
	let rec typehasVar (x : var) (ty : typ) : bool =
		(match ty with
		| TInt -> false
		| TBool -> false
		| TString -> false
		| TPair (a,b) -> (typehasVar x a) || (typehasVar x b)
		| TLoc l -> (typehasVar x l)
		| TFun (a,b) -> (typehasVar x a) || (typehasVar x b)
		| TVar v -> ((String.compare v x) == 0)
		)
	in 
    (match (ty1,ty2) with
	| (TVar x, t) ->
		if (hasSameType (TVar x) t) then emptysub
		else (if (not (typehasVar x t)) then (substitution x t)
			  else raise (M.TypeError "There is no matched type"))
	| (t, TVar x) -> unify (TVar x) t
	| (TFun (a1,a2), TFun (b1,b2)) ->
		let f1 = (unify a1 b1) in
		let f2 = (unify (f1 a2) (f1 b2)) in
		(f2 @@ f1)
	| (TPair (a1,a2), TPair(b1,b2)) -> 
		let u1 = (unify a1 b1) in
		let u2 = (unify (u1 a2) (u1 b2)) in
		(u2 @@ u1)
	| (TLoc a, TLoc b) -> (unify a b)
	| (a,b) ->
		if (hasSameType a b) then emptysub
		else raise (M.TypeError "There is no matched type")
	)
(*
   함수 getTypeEqu : Env와 Exp와 type을 받아서, typeequ -> typeequ를 리턴.
   여기서 Env : (string -> type)으로 나타내어지는 함수
        Exp : M.exp
		type : 이 모듈에서 정의된 type (M.type이 아님!) 을 나타냄.
   getTypeequ를 실행하면 방정식 전체를 얻어올 수 있는 함수를 얻어낼 수 있음.
   그 함수에다 빈 typeequ를 먹여주면 ([]) 전체 방정식을 내뱉게 된다.
   
   처음에는 getTypeequ를 Env,Exp,Type과 typeequ를 받아서 typeequ를 내뱉는 함수로 짰는데
   중복때문에 결과로 겁나게 더러운 리스트가 나옴..
   -> (typeequ -> typeequ)를 리턴하게 수정.
*)		
let rec getTypeEqu (env : typenv) (ex : M.exp) (t : typ) : (typeEquation -> typeEquation) =
	let cstU (ty1 : typ) (ty2 : typ) = (fun cst -> (Unique (ty1,ty2))::cst) in
	let cstOr (ty : typ) (tylist : typ list) = (fun cst -> (Or (ty,tylist)) :: cst) in	
	match ex with
	| M.CONST (M.S s) -> (cstU t TString) 
	| M.CONST (M.N n) -> (cstU t TInt) 
	| M.CONST (M.B b) -> (cstU t TBool) 
	| M.VAR x -> (cstU t (env x))
	| M.FN (x,e) -> 
		let n1 = new_var() in
		let n2 = new_var() in 
		let t1 = (TVar n1) in
		let t2 = (TVar n2) in 
		(cstU t (TFun (t1,t2))) @@ (getTypeEqu (env @+ (x, t1)) e t2)
	| M.APP (e1,e2) ->
		let n1 = new_var() in 
		let t1 = (TVar n1) in
		(getTypeEqu env e1 (TFun (t1, t))) @@ (getTypeEqu env e2 t1)
	| M.LET (decl, e) ->
		(match decl with
		| M.VAL (x_id,x_exp) -> 
			let n1 = new_var() in 
			let t1 = (TVar n1) in
			(getTypeEqu env x_exp t1) @@ (getTypeEqu (env @+ (x_id,t1)) e t)  
		| M.REC (f_id,a_id,f_exp) -> 
			let n1 = new_var() in
			let n2 = new_var() in 
			let t1 = (TVar n1) in
			let t2 = (TVar n2) in 
		    (getTypeEqu ((env @+ (a_id,t1)) @+ (f_id,TFun (t1,t2))) f_exp t2) @@ (getTypeEqu (env @+ (f_id, TFun (t1,t2))) e t)
		)
	| M.IF (e1, e2, e3) -> 
		let n1 = new_var() in 
		let t1 = (TVar n1) in
		(cstU t t1) @@ (getTypeEqu env e1 (TBool)) @@ (getTypeEqu env e2 t1) @@ (getTypeEqu env e3 t1)    
	| M.READ -> (cstU t TInt)
	| M.WRITE e -> (cstOr t [TInt;TBool;TString]) @@ (getTypeEqu env e t)
	| M.MALLOC e -> 
	let n1 = new_var() in 
	let t1 = (TVar n1) in
	(cstU t (TLoc t1)) @@ (getTypeEqu env e t1) 
	| M.ASSIGN (e1,e2) ->
	(getTypeEqu env e1 (TLoc t)) @@ (getTypeEqu env e2 t)
	| M.BANG e -> 
	(getTypeEqu env e (TLoc t))
	| M.SEQ (e1,e2) ->
	let n1 = new_var() in 
	let t1 = (TVar n1) in
	(getTypeEqu env e1 t1) @@ (getTypeEqu env e2 t)
	| M.BOP (op, e1, e2) -> 
	(match op with
		| M.ADD -> (cstU t TInt) @@ (getTypeEqu env e1 TInt) @@ (getTypeEqu env e2 TInt)
		| M.SUB -> (cstU t TInt) @@ (getTypeEqu env e1 TInt) @@ (getTypeEqu env e2 TInt)
		| M.AND -> (cstU t TBool) @@ (getTypeEqu env e1 TBool) @@ (getTypeEqu env e2 TBool)
		| M.OR -> (cstU t TBool) @@ (getTypeEqu env e1 TBool) @@ (getTypeEqu env e2 TBool)
		| M.EQ -> 
		let n1 = new_var() in
		let n2 = new_var() in 
		let t1 = (TVar n1) in
		let t2 = (TVar n2) in 
		(cstU t TBool) @@ (cstOr t1 [TInt;TBool;TString;TLoc t2])
		@@ (getTypeEqu env e1 t1) @@ (getTypeEqu env e2 t1)
		)
	| M.PAIR (e1, e2) -> 
	let n1 = new_var() in
	let n2 = new_var() in 
	let t1 = (TVar n1) in
	let t2 = (TVar n2) in 
	(cstU t (TPair (t1,t2))) @@ (getTypeEqu env e1 t1) @@ (getTypeEqu env e2 t2)
	| M.FST e ->
	let n1 = new_var() in
	let n2 = new_var() in 
	let t1 = (TVar n1) in
	let t2 = (TVar n2) in  
	(cstU t t1) @@ (getTypeEqu env e (TPair (t1,t2)))
	| M.SND e -> 
	let n1 = new_var() in
	let n2 = new_var() in 
	let t1 = (TVar n1) in
	let t2 = (TVar n2) in 
	(cstU t t2) @@ (getTypeEqu env e (TPair (t1,t2)))


let isUnique (cst : constraints) : bool =
	match cst with
	| Unique _ -> true
	| Or _ -> false

let sortEquation (cstl : typeEquation) : typeEquation =
	let (u,o) = (List.partition isUnique cstl) in
	u @ o

let rec solveTypeEquation (sub : subst) (eq : typeEquation) : subst =
	match eq with
	| [] -> sub
	| (Unique (t1,t2) :: tl) -> (solveTypeEquation (unify (sub t1) (sub t2) @@ sub) tl)
	| (Or (t,tylist) :: tl) -> 
				let rec doAllU (tylst : typ list) : subst = 
				(match tylst with
					| [] -> raise (M.TypeError "OR can't match anything")
					| hd::ttl -> (try 
								(solveTypeEquation sub ((Unique (t,hd))::tl))
								 with _ -> (doAllU ttl))
				) in 
			    (doAllU tylist)

(*
  (* types in M  *)
  type types = TyInt                     (* integer type *)
             | TyBool                    (* boolean type *)
             | TyString                  (* string type *)
             | TyPair of types * types   (* pair type *)
             | TyLoc of types            (* location type *)
             | TyArrow of types * types  (* function type *)
*)
let rec typeconvert (t : typ) : M.types =
	match t with
	| TInt -> M.TyInt
	| TBool -> M.TyBool
	| TString -> M.TyString
	| TPair (a,b) -> M.TyPair (typeconvert a,typeconvert b)
	| TLoc l -> (M.TyLoc (typeconvert l))
	| TFun (a,b) -> M.TyArrow (typeconvert a, typeconvert b)
	| TVar v -> raise (M.TypeError "converting failed!")

let rec typeToString (t : typ) : string =
	(match t with
	| TInt -> " TInt "
	| TBool -> " TBool "
	| TString -> " TString "
	| TPair (a,b) -> " TPair (" ^ typeToString a ^ "," ^ typeToString b ^ ") "
	| TLoc l -> " TLoc (" ^ typeToString l ^ ") "
	| TFun (a,b) -> " TFun (" ^ typeToString a ^ "," ^ typeToString b ^ ") "
	| TVar v -> " TVar " ^ v
	)

let rec printtypeequ (te : typeEquation) : unit =
	(match te with
	| [] -> print_string "Equation End\n" 
	| (Unique (t1,t2) :: tl) -> 
		let str = "Unique\n" ^ (typeToString t1) ^ "\n=\n" ^ (typeToString t2) ^ "\n and \n" in 
		let _ = print_string str in (printtypeequ tl)
	| (Or (t,tylist) :: tl) -> 
		let rec doStr (tylst : typ list) : unit = 
					(match tylst with
					| [] -> print_string "OR End\n" 
					| hd::ttl -> 
							let str = "Ormem\n" ^ (typeToString t) ^ "\n=\n" ^ (typeToString hd) ^ "\n and \n" in 
						    let _ = print_string str in (doStr ttl)
					) in 
			    let _ = (doStr tylist) in (printtypeequ tl) 
	)
(* 디버그용 프린트 함수 *)

(* TODO : Implement this function *)
let check : M.exp -> M.types = (fun exp ->
   let n1 = new_var() in
   let t1 = (TVar n1) in 
   let typeEqu = sortEquation ((getTypeEqu emptyEnv exp t1) []) in
   let s = (solveTypeEquation emptysub typeEqu) in
   (typeconvert (s t1))
  )
  
(* 문제점 : let a = malloc 1 in !a 와 같은 reference-dereference 연산을 하면 스택오버플로우로 터짐
   문제점 2 : let val b = malloc (fn x => (1+x)) in b 와 같은 함수의 리턴값은 Loc (int -> int) 여야 하는데 Loc (int)가 나옴.
   let val b = fn x => (1+x) in b 와 같은 코드는 정상적인 리턴값 (fun int -> int)를 리턴함.
   문제점 3 : let a = malloc malloc 1 in a 와 같은 코드도 스택오버플로우로 터짐.
   
   해결함 -> 함수 하나 잘못짜서 로케이션 들어가면 내부에서 재귀적으로 계속 받고있었음. 이러니까 스택이 터지지.. 도대체 무엇...?
*)

   
   
   