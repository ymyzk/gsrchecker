open Environment
open Syntax
open Typing

exception Type_error of string

(* ty -> ty -> bool *)
let rec check_consistency s t = match s, t with
| (s, t) when s = t -> true (* TCRefl *)
| (s, TyDyn) -> true (* TCUnR *)
| (TyDyn, s) -> true (* TCUnR *)
| (TyFun (s1, a1, t1, b1), TyFun (s2, a2, t2, b2)) ->
    check_consistency s1 s2 &&
    check_consistency a1 a2 &&
    check_consistency t1 t2 &&
    check_consistency b1 b2
| _ -> false (* s is not type consistent with t *)

let rec check env e b = match e with
  | Var (x) -> begin
      try
        let t = Environment.find x env in (b, t)
      with
        Not_found -> raise (Type_error "'x' is not found in the environment")
    end
  | Fun (fb, fx, ft1, fe) ->
      let env' = Environment.add fx ft1 env in
      let a, t2 = check env' fe fb in
      (b, TyFun (ft1, a, t2, fb))
  | App (e1, e2) ->
      let g, ft = check env e1 b in
      begin
        match ft with
        | TyDyn -> begin
          let _ = check env e2 g in
          (TyDyn, TyDyn)
        end
        | TyFun (ft1, fa, ft2, fb) ->
            let b', t1' = check env e2 g in
            begin
              match (check_consistency ft1 t1', check_consistency fb b') with
              | (true, true) -> (fa, ft2)
              | _ -> raise @@ Type_error "inconsistent"
            end
        | _ -> raise @@ Type_error "invalid application"
      end
  | Sft (k, TyFun (t, d, a, d'), se) when d = d' ->
      let env' = Environment.add k (TyFun (t, d, a, d)) env in
      let g', g = check env' se b in
      if check_consistency g g' then
        (a, t)
      else
        raise @@ Type_error "shift error"
  | Rst (re, t) ->
      let g', g = check env re t in
      if check_consistency g g' then
        (b, t)
      else
        raise @@ Type_error "reset error"
  | _ -> raise @@ Type_error "invalid expression"
