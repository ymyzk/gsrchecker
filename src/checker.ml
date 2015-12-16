open Typing

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
