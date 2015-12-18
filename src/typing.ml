type basety =
  | TyBool
  | TyInt

type ty =
  | TyBase of basety
  | TyFun of ty * ty * ty * ty
  | TyDyn
