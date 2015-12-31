open Checker
open Environment
open Printer
open Syntax
open Typing

let check_and_print env e b =
  print_endline "Input: ";
  print_endline @@ "  Γ: " ^ sprint_type_environment env;
  print_endline @@ "  e: " ^ sprint_exp e;
  print_endline @@ "  β: " ^ sprint_type b;
  print_endline "Check: ";
  begin
    try
      let a, t = check ~mode:CheckText env e b in
      print_endline "OK:";
      print_endline @@ "  α: " ^ sprint_type a;
      print_endline @@ "  τ: " ^ sprint_type t
    with
      Type_error e -> print_endline @@ "NG: " ^ e
  end;
  print_endline "-----"

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
      (* 123: int *)
      let env = Environment.empty in
      check_and_print env (Cst (CstInt 123)) tyint
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
  begin
    fun () ->
      let env = Environment.empty in
      let e = App (Fun (TyDyn, "x", TyDyn, Var "x"), Cst (CstInt 123)) in
      check_and_print env e TyDyn
  end;
  begin
    fun () ->
      let env = Environment.empty in
      let e = App (Fun (TyDyn, "y", TyDyn, Var "y"), Fun (TyDyn, "x", TyDyn, Var "x")) in
      check_and_print env e TyDyn
  end
]

let () = List.iter (fun case -> case ()) cases
