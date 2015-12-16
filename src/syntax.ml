open Typing

type id = string

type exp =
  | Var of id
  | Fun of ty * id * ty * exp
  | App of exp * exp
  | Sft of id * ty * exp
  | Rst of exp * ty
