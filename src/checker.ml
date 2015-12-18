open Environment
open Syntax
open Typing

exception Type_error of string

type check_mode =
  | CheckSilent
  | CheckText

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

let check ?(mode=CheckSilent) env e b =
  let rec check env e b =
    let open Printer in
    let print env a e t b r =
      match mode with
      | CheckSilent -> ()
      | CheckText -> begin
          print_string @@ sprint_type_environment env;
          print_string "; ";
          print_string @@ sprint_type a;
          print_string " |- ";
          print_string @@ sprint_exp e;
          print_string ": ";
          print_string @@ sprint_type t;
          print_string "; ";
          print_string @@ sprint_type b;
          print_string " (";
          print_string r;
          print_endline ")"
        end
    in
    match e with
    | Var (x) -> begin
        try
          let t = Environment.find x env in
          let a = b in
          print env a e t b "GTVar";
          (a, t)
        with
          Not_found -> raise (Type_error "'x' is not found in the environment")
      end
    | Fun (fb, fx, ft1, fe) ->
        let env' = Environment.add fx ft1 env in
        let a2, t2 = check env' fe fb in
        let a = b in
        let t = TyFun (ft1, a2, t2, fb) in
        print env a e t b "GTFun";
        (a, t)
    | App (e1, e2) ->
        let g, ft = check env e1 b in
        begin
          match ft with
          | TyDyn -> begin
            let _ = check env e2 g in
            let a = TyDyn in
            let t = TyDyn in
            print env a e t b "GTApp1";
            (a, t)
          end
          | TyFun (ft1, fa, ft2, fb) ->
              let b', t1' = check env e2 g in
              begin
                match (check_consistency ft1 t1', check_consistency fb b') with
                | (true, true) -> begin
                  let a = fa in
                  let t = ft2 in
                  print env a e t b "GTApp2";
                  (a, t)
                end
                | _ -> raise @@ Type_error "inconsistent"
              end
          | _ -> raise @@ Type_error "invalid application"
        end
    | Sft (k, TyFun (t, d, a, d'), se) when d = d' ->
        let env' = Environment.add k (TyFun (t, d, a, d)) env in
        let g', g = check env' se b in
        if check_consistency g g' then
          begin
            print env a e t b "GTShift";
            (a, t)
          end
        else
          raise @@ Type_error "shift error"
    | Rst (re, t) ->
        let g', g = check env re t in
        if check_consistency g g' then
          let a = b in
          print env a e t b "GTReset";
          (b, t)
        else
          raise @@ Type_error "reset error"
    | _ -> raise @@ Type_error "invalid expression"
  in
  check env e b
