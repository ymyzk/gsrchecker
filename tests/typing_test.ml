open OUnit2

open Typing

let test_sprint_type =
  let tybool = TyBase TyBool in
  let tyint = TyBase TyInt in
  let f0 = TyFun (TyDyn, TyDyn, TyDyn, TyDyn) in
  let f1 = TyFun (tyint, tybool, tyint, tybool) in
  let f2 = TyFun (f0, tybool, f1, tybool) in
  let check_list = [
    TyDyn, "?";
    tybool, "bool";
    tyint, "int";
    f0, "?/?->?/?";
    f1, "int/bool->int/bool";
    f2, "(?/?->?/?)/bool->(int/bool->int/bool)/bool"
  ]
  in

  List.map
   (fun (t, r) ->
      let title = "expects " ^ r in
      title >:: (fun test_ctxt -> assert_equal r @@ sprint_type t))
    check_list

let suite = [
  "test_sprint_type">::: test_sprint_type
]
