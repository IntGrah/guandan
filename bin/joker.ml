open Base

type t = Black | Red [@@deriving show, eq, ord]

let tests () =
  assert (compare Red Red = 0);
  assert (compare Red Black > 0);
  assert (compare Black Red < 0);
  assert (compare Black Black = 0);
  Stdio.print_endline "Joker: tests passed"
