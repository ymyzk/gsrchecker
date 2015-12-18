open Syntax
open Typing

module Environment = Map.Make (
  struct
    type t = id
    let compare = compare
  end
)
