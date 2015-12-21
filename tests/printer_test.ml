open OUnit2

open Printer
open Syntax
open Typing

let tybool = TyBase TyBool
let tyint = TyBase TyInt

let test_sprint_type_text =
  let f0 = TyFun (TyDyn, TyDyn, TyDyn, TyDyn) in
  let f1 = TyFun (tyint, tybool, tyint, tybool) in
  let f2 = TyFun (f0, tybool, f1, tybool) in
  let check_list = [
    TyDyn, "*";
    tybool, "bool";
    tyint, "int";
    f0, "*/*->*/*";
    f1, "int/bool->int/bool";
    f2, "(*/*->*/*)/bool->(int/bool->int/bool)/bool"
  ]
  in
  List.map
   (fun (t, r) ->
      let title = "expects: " ^ r in
      title >:: (fun test_ctxt ->
        assert_equal r @@ sprint_type ~mode:PrintText t))
    check_list

let test_sprint_exp_text =
  let check_list = [
    Var "x", "x";
    App (Var "f", Var "x"), "f x";
    Fun (tybool, "x", tyint, Var "x"), "λ^{bool}(x:int).x";
    Sft ("k", tyint, App (Var "k", Var "x")), "S(k:int).k x";
    Rst (Var "x", tyint), "<x>^{int}";
  ]
  in
  List.map
   (fun (e, r) ->
      let title = "expects: " ^ r in
      title >:: (fun test_ctxt ->
        assert_equal r @@ sprint_exp ~mode:PrintText e))
    check_list

let suite = [
  "test_sprint_type">::: [
    "text">::: test_sprint_type_text
  ];
  "test_sprint_exp">::: [
    "text">::: test_sprint_exp_text
  ]
]
