open Syntax
open Typing

module Environment = Map.Make (
  struct
    type t = id
    let compare = compare
  end
)

let sprint_type_environment env =
  let bindings = Environment.bindings env in
  String.concat ", " @@
    List.map (fun (id, ty) -> id ^ ":" ^ sprint_type ty) bindings
