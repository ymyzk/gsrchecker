open Environment
open Syntax
open Typing

let rec check env e b = match e with
  | Var (x) -> (b, TyDyn)
  | Fun (fb, fx, ft1, fe) ->
      let env' = Environment.add fx ft1 env in
      let a, t2 = check env' e b in
      (b, TyFun (ft1, a, t2, fb))
  | Sft (k, TyFun (t, d, a, d'), e) when d = d' ->
      let env' = Environment.add k (TyFun (t, d, a, d')) env in
      let g', g = check env' e, b in
      (* check consistency *)
      ignore g;
      (a, t)
  | _ -> (TyDyn, TyDyn)

let print (env, a, e, t, b) =
  let rec sprint_type = function
    | TyBase (TyBool) -> "bool"
    | TyBase (TyInt) -> "int"
    | TyFun (w, x, y, z) ->
        (sprint_type w) ^ "/" ^ (sprint_type x) ^ "->" ^
        (sprint_type y) ^ "/" ^ (sprint_type z)
    | TyDyn -> "?"
  in
  let sprint_env env =
    let bindings = Environment.bindings env in
    String.concat ", " @@
      List.map (fun (id, ty) -> id ^ ":" ^ sprint_type ty) bindings
  in
  let rec sprint_exp = function
    | Var (x) -> x
    | Fun (b, x, t, e) ->
        "λ(" ^ sprint_type b ^ ")" ^
        x ^ ":" ^ sprint_type t ^ "." ^
        sprint_exp e
    | App (e1, e2) ->
        sprint_exp e1 ^ " " ^ sprint_exp e2
    | Sft (k, t, e) ->
        "S" ^ k ^ ":" ^ sprint_type t ^ "." ^ sprint_exp e
    | Rst (e, t) ->
        "<" ^ sprint_exp e ^ ">^" ^ sprint_type t
  in
  print_endline "OK";
  print_endline @@ "Gamma: " ^ sprint_env env;
  print_endline @@ "e: " ^ sprint_exp e;
  print_endline @@ "alpha: " ^ sprint_type a;
  print_endline @@ "tau: " ^ sprint_type t;
  print_endline @@ "beta: " ^ sprint_type b

let () =
(*   let x = fresh_var () in *)
  let x = "x" in
  let env = Environment.singleton x (TyBase TyInt) in
  let e = Var x in
  let b = TyBase TyInt in
  let a, t = check env e b in
  ignore @@ print (env, a, e, t, b)
