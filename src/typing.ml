type basety =
  | TyBool
  | TyInt

type ty =
  | TyBase of basety
  | TyFun of ty * ty * ty * ty
  | TyDyn

let sprint_type ty =
  let rec sprint_type = function
    | TyBase (base_type) -> (match base_type with
      | TyBool -> "bool"
      | TyInt -> "int")
    | TyFun (w, x, y, z) ->
        "(" ^ (sprint_type w) ^ "/" ^ (sprint_type x) ^ "->" ^
        (sprint_type y) ^ "/" ^ (sprint_type z) ^ ")"
    | TyDyn -> "?"
  in
  match ty with
  | TyFun (w, x, y, z) ->
      (sprint_type w) ^ "/" ^ (sprint_type x) ^ "->" ^
      (sprint_type y) ^ "/" ^ (sprint_type z)
  | _ -> sprint_type ty
