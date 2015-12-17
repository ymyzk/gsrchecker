open Typing

type id = string

type exp =
  | Var of id
  | Fun of ty * id * ty * exp
  | App of exp * exp
  | Sft of id * ty * exp
  | Rst of exp * ty

let rec sprint_exp = function
  | Var (x) -> x
  | Fun (b, x, t, e) ->
      "\\lambda^{" ^ sprint_type b ^ "}" ^
      x ^ ":" ^ sprint_type t ^ "." ^
      sprint_exp e
  | App (e1, e2) ->
      sprint_exp e1 ^ " " ^ sprint_exp e2
  | Sft (k, t, e) ->
      "\\mathcal{S}" ^ k ^ ":" ^ sprint_type t ^ "." ^ sprint_exp e
  | Rst (e, t) ->
      "\\langle " ^ sprint_exp e ^ " \\rangle^{" ^ sprint_type t ^ "}"
