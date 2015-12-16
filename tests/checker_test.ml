open OUnit2

open Checker
open Typing

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

let suite = [
  "test_check_consistency">::: test_check_consistency
]
