open Base
open Guandan.Player

let () =
  let store : int store = Store (1, 2, 3, 4) in
  assert (store |> get A = 1);
  assert (store |> set A 5 |> get A = 5)
