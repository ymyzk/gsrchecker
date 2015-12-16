type basety =
  | TyBool
  | TyInt

type ty =
  | TyBase of basety
  | TyFun of ty * ty * ty * ty
  | TyDyn

let fresh_var =
  let counter = ref 0 in
  let body () =
    let v = !counter in
      counter := v + 1;
      v
  in body
