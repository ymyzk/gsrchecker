open Printf

open Environment
open Syntax
open Typing

type print_mode =
  | PrintText
  | PrintTeX

let sprint_type ?(mode=PrintText) ty =
  let rec sprint_type = function
    | TyBase (base_type) -> (match base_type with
      | TyBool -> "bool"
      | TyInt -> "int")
    | TyFun (w, x, y, z) ->
        sprintf "(%s/%s->%s/%s)"
          (sprint_type w) (sprint_type x)
          (sprint_type y) (sprint_type z)
    | TyDyn -> "?"
  in
  match ty with
  | TyFun (w, x, y, z) ->
      sprintf "%s/%s->%s/%s"
        (sprint_type w) (sprint_type x)
        (sprint_type y) (sprint_type z)
  | _ -> sprint_type ty

let sprint_type_environment ?(mode=PrintText) env =
  if Environment.is_empty env then
    "・"
  else
    let bindings = Environment.bindings env in
    String.concat "," @@
      List.map (fun (id, ty) -> id ^ ":" ^ sprint_type ~mode ty) bindings

let sprint_exp ?(mode=PrintText) e =
  let sprint_type = sprint_type ~mode in
  let rec sprint_exp = function
    | Var (x) -> x
    | Fun (b, x, t, e) ->
        sprintf "λ^{%s}(%s:%s).%s" (sprint_type b) x (sprint_type t) (sprint_exp e)
    | App (e1, e2) ->
        sprintf "%s %s" (sprint_exp e1) (sprint_exp e2)
    | Sft (k, t, e) ->
        sprintf "S(%s:%s).%s" k (sprint_type t) (sprint_exp e)
    | Rst (e, t) ->
        sprintf "<%s>^{%s}" (sprint_exp e) (sprint_type t)
  in
  sprint_exp e
