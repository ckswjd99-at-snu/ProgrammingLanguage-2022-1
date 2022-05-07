(*
 * SNU 4190.310 Programming Languages (Spring 2022)
 *
 * Lambda Calculus
 *)
open Lambda
module Evaluator =
  struct
	exception Error of string

  module StrSet = Set.Make(struct type t = string let compare = Pervasives.compare end)

  type substitution = (string * lexp) list

  let rec new_var_helper n var_set = 
    let var_to_try = "#" ^ (string_of_int n) in
    if not (StrSet.mem var_to_try var_set) then
      var_to_try
    else
      new_var_helper (n + 1) var_set

  let new_var recommend var_set = 
    if not (StrSet.mem recommend var_set) then
      recommend
    else
      new_var_helper 0 var_set

  let rec free_vars = function
    | Var x -> StrSet.singleton x
    | Lam (x, e) -> StrSet.remove x (free_vars e)
    | App (e1, e2) -> StrSet.union (free_vars e1) (free_vars e2)

  let rec subs: (string * lexp) -> lexp -> lexp = fun (var, var_exp) exp ->
  match exp with
  | Var(x) -> if var = x then var_exp else exp
  | Lam(x,e) -> 
      let var_set = StrSet.singleton var in
      let var_set = StrSet.union var_set (free_vars var_exp) in
      let var_set = StrSet.union var_set (free_vars exp) in
      let x' = new_var x var_set in
      let e' = subs (x, Var x') e in
      let e'' = subs (var, var_exp) e' in
      Lam (x', e'')
  | App(e1,e2) -> App(subs (var, var_exp) e1, subs (var, var_exp) e2)

  let subst: substitution * lexp -> lexp = fun (sub, exp) ->
  List.fold_left (
    fun acc sub_ele -> subs sub_ele acc
    )
     exp sub
  
  module Pp =
    struct
      let rec pp exp =
      match exp with
        Var s -> print_string s
      | Lam (s, e) -> print_string "\\"; print_string (s^"."); pp e; print_string ""
      | App (e1, e2) -> print_string "("; pp e1; print_string ") ("; pp e2; print_string ")"
    end

  let reduce : lexp -> lexp
	= fun exp -> (
    let rec reducer (lexpr : lexp) : lexp = (
      match lexpr with
      | Var x -> Var x
      | Lam (x, e) -> Lam (x, reducer e)
      | App (Lam (x, e1), e2) -> subst ([(x, e2)], e1)
      | App (App (e1, e2), e3) -> (
        let tmp_lexp = App(e1, e2) in
				let reduced_lexp = reducer (tmp_lexp) in
				if reduced_lexp = tmp_lexp 
				then App(tmp_lexp, reducer e3)
				else App(reduced_lexp, e3)
      )
      | App (e1, e2) -> App (reducer e1, reducer e2)
    ) in
    let rec reduce_recursive lexpr = (
      let reduced = reducer lexpr in
      if lexpr = reduced then lexpr else reduce_recursive reduced
    ) in

    reduce_recursive exp
  )

  end
