open Checker
open Environment
open Printer
open Syntax
open Typing

let check_and_print env e b =
  try
    ignore @@ check ~mode:CheckText env e b;
    print_endline "OK";
  with
    Type_error e -> print_endline @@ "NG: " ^ e

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
  begin
    fun () ->
      (* f:?, x:int *)
      let env = Environment.singleton "f" TyDyn in
      let env = Environment.add "x" (TyBase TyInt) env in
      let e = App (Var "f", Var "x") in
      check_and_print env e tyint
  end;
  begin
    fun () ->
      (* f:int/bool->int/? x:int *)
      let env = Environment.singleton "f" @@ TyFun (tyint, tybool, tyint, TyDyn) in
      let env = Environment.add "x" (TyBase TyInt) env in
      let e = App (Var "f", Var "x") in
      check_and_print env e tyint
  end;
  begin
    fun () ->
      (* f:int/bool->int/bool x:int *)
      let env = Environment.singleton "f" @@ TyFun (tyint, tybool, tyint, tybool) in
      let env = Environment.add "x" (TyBase TyInt) env in
      let e = App (Var "f", Var "x") in
      check_and_print env e tyint
  end;
]

let () = List.iter (fun case -> case ()) cases
