open Checker
open Environment
open Syntax
open Typing

let print (env, a, e, t, b) =
  print_string @@ sprint_type_environment env;
  print_string "; ";
  print_string @@ sprint_type a;
  print_string " \\vdash_{s/r}^? ";
  print_string @@ sprint_exp e;
  print_string ":";
  print_string @@ sprint_type t;
  print_string "; ";
  print_string @@ sprint_type b;
  print_endline ""

let check_and_print env e b =
  let a, t = check env e b in
  ignore @@ print (env, a, e, t, b)

let tybool = TyBase TyBool
let tyint = TyBase TyInt

let cases = [
  begin
    fun () ->
    (* x: int *)
    let env = Environment.singleton "x" (TyBase TyInt) in
    check_and_print env (Var "x") tyint
  end;
  begin
    fun () ->
    (* <x>^int *)
    let env = Environment.singleton "x" (TyBase TyInt) in
    let e = Rst (Var "x", tyint) in
    check_and_print env e tyint
  end;
  begin
    fun () ->
    (* <Sk:?.x>^int *)
    let env = Environment.singleton "x" (TyBase TyInt) in
    let e = Rst (Sft ("k", TyFun(TyDyn, TyDyn, TyDyn, TyDyn), Var "x"), tyint) in
    check_and_print env e tyint
  end;
  begin
    fun () ->
    (* <Sk:int/int->int/int.x>^int *)
    let env = Environment.singleton "x" (TyBase TyInt) in
    let e = Rst (Sft ("k", TyFun(tyint, tyint, tyint, tyint), Var "x"), tyint) in
    check_and_print env e tyint
  end;
  begin
    fun () ->
    (* <Sk:?.\x:int.x>^int *)
    let env = Environment.empty in
    let e = Rst (Sft ("k", TyFun(TyDyn, TyDyn, TyDyn, TyDyn), Fun (tyint, "x", tyint, Var "x")), TyFun(tyint, TyDyn, tyint, TyDyn)) in
    check_and_print env e tyint
  end;
]

let () = List.iter (fun case -> case (); print_endline "\\\\") cases
