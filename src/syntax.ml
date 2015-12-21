open Typing

type id = string

type const =
  | CstBool of bool
  | CstInt of int

type exp =
  | Var of id
  | Cst of const
  | Fun of ty * id * ty * exp
  | App of exp * exp
  | Sft of id * ty * exp
  | Rst of exp * ty
