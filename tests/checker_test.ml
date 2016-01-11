open OUnit2

open Checker
open Environment
open Printer
open Syntax
open Typing

let test_match_function =
  let tybool = TyBase TyBool in
  let tyint = TyBase TyInt in
  let f0 = TyFun (TyDyn, TyDyn, TyDyn, TyDyn) in
  let f1 = TyFun (tyint, tybool, tyint, tybool) in
  let check_list = [
    TyDyn, f0;
    f0, f0;
    f1, f1;
  ] in
  List.map
   (fun (s, t) ->
      let title =
        "fun(" ^
        (sprint_type s) ^
        ")=" ^
        (sprint_type t)
      in
      title >:: (fun test_ctxt -> assert_equal t @@ match_function s))
    check_list

let test_check_consistency =
  let tybool = TyBase TyBool in
  let tyint = TyBase TyInt in
  let f0 = TyFun (TyDyn, TyDyn, TyDyn, TyDyn) in
  let f1 = TyFun (tyint, TyDyn, TyDyn, TyDyn) in
  let f2 = TyFun (tyint, tybool, TyDyn, TyDyn) in
  let f3 = TyFun (tyint, tybool, tyint, TyDyn) in
  let f4 = TyFun (tyint, tybool, tyint, tybool) in
  let check_list = [
    TyDyn, TyDyn, true;
    tyint, tyint, true;
    tyint, tybool, false;
    f0, f0, true;
    f0, f1, true;
    f0, f2, true;
    f0, f3, true;
    f0, f4, true;
  ] in
  List.map
   (fun (s, t, r) ->
      let title =
        sprint_type s ^
        (if r then " ~ " else " !~ ") ^
        sprint_type t
      in
      title >:: (fun test_ctxt -> assert_equal r @@ check_consistency s t))
    check_list

let test_check_suite () =
  let tyint = TyBase TyInt in
  let tybool = TyBase TyBool in
  [
  "GTVar int">:: begin
    fun test_ctxt ->
      let env = Environment.singleton "x" tyint in
      let x = Var "x" in
      let b = tyint in
      assert_equal (tyint, tyint) @@ check env x b
  end;
  "GTVar not exists">:: begin
    fun test_ctxt ->
      let env = Environment.empty in
      let x = Var "x" in
      let b = TyBase TyInt in
      assert_raises
        (Type_error "'x' is not found in the environment")
        (fun () -> check env x b)
  end;
  "GTConst bool">:: begin
    fun test_ctxt ->
      let env = Environment.empty in
      let c = Cst (CstInt 123) in
      let b = tyint in
      assert_equal (tyint, tyint) @@ check env c b
  end;
  "GTConst int">:: begin
    fun test_ctxt ->
      let env = Environment.empty in
      let c = Cst (CstBool false) in
      let b = tyint in
      assert_equal (tyint, tybool) @@ check env c b
  end;
  "GTFun int/bool->int/bool">:: begin
    fun test_ctxt ->
      let env = Environment.empty in
      let x = Var "x" in
      let e = Fun (tybool, "x", tyint, x) in
      let expects = (TyDyn, TyFun (tyint, tybool, tyint, tybool)) in
      assert_equal expects @@ check env e TyDyn
  end;
  "GTFun ?/int->?/int">:: begin
    fun test_ctxt ->
      let env = Environment.empty in
      let x = Var "x" in
      let e = Fun (tyint, "x", TyDyn, x) in
      let expects = (tybool, TyFun (TyDyn, tyint, TyDyn, tyint)) in
      assert_equal expects @@ check env e tybool
  end;
  "GTApp fail">:: begin
    fun test_ctxt ->
      let env = Environment.singleton "x" tyint in
      let env = Environment.add "y" tyint env in
      let e = App (Var "x", Var "y") in
      assert_raises
        (Type_error "cannot match function")
        (fun () -> check env e tybool)
  end;
  "GTApp1">:: begin
    fun test_ctxt ->
      let env = Environment.singleton "df" TyDyn in
      let env = Environment.add "x" tyint env in
      let e = App (Var "df", Var "x") in
      let expects = (TyDyn, TyDyn) in
      assert_equal expects @@ check env e tybool
  end;
  "GTApp2 1">:: begin
    fun test_ctxt ->
      let env = Environment.singleton "f" (TyFun (tyint, tybool, tyint, tyint)) in
      let env = Environment.add "x" tyint env in
      let e = App (Var "f", Var "x") in
      let expects = (tybool, tyint) in
      assert_equal expects @@ check env e tyint
  end;
  "GTApp2 2">:: begin
    fun test_ctxt ->
      let env = Environment.singleton "f" (TyFun (tyint, tybool, tyint, tyint)) in
      let env = Environment.add "x" tyint env in
      let e = App (Var "f", Var "x") in
      let expects = (tybool, tyint) in
      assert_equal expects @@ check env e TyDyn
  end;
  "GTApp2 beta is not consistent">:: begin
    fun test_ctxt ->
      let env = Environment.singleton "f" (TyFun (tyint, tybool, tyint, tyint)) in
      let env = Environment.add "x" tyint env in
      let e = App (Var "f", Var "x") in
      assert_raises
        (Type_error "inconsistent")
        (fun () -> check env e tybool)
  end;
  "GTShift invalid continuation type (not function)">:: begin
    fun test_ctxt ->
      let env = Environment.singleton "x" tyint in
      let e = Sft ("k", tyint, Var "x") in
      assert_raises
        (Type_error "cannot match function")
        (* More better message *)
        (* (Type_error "shift error: the captured continuation type must be a function type") *)
        (fun () -> check env e tyint)
  end;
  "GTShift invalid continuation type (answer types do not match)">:: begin
    fun test_ctxt ->
      let env = Environment.singleton "x" tyint in
      let e = Sft ("k", TyFun (tyint, tyint, tyint, tybool), Var "x") in
      assert_raises
        (Type_error "shift error: answer types of the captured continuation must be same")
        (fun () -> check env e tyint)
  end;
  "GTReset">:: begin
    fun test_ctxt ->
      let env = Environment.singleton "x" tyint in
      let x = Var "x" in
      let e = Rst (x, tyint) in
      let expects = (tybool, tyint) in
      assert_equal expects @@ check env e tybool
  end;
]

let suite = [
  "test_match_function">::: test_match_function;
  "test_check_consistency">::: test_check_consistency;
  "test_check">::: test_check_suite ();
]
