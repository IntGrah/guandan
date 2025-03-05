open Base

type t = A | B | C | D [@@deriving show, eq]
type team = AC | BD [@@deriving show]

let next : t -> t = function A -> B | B -> C | C -> D | D -> A
let teammate : t -> t = Fn.compose next next
let team : t -> team = function A | C -> AC | B | D -> BD

type 'a store = Store of 'a * 'a * 'a * 'a [@@deriving show]

let init (x : 'a) : 'a store = Store (x, x, x, x)

let get (p : t) (Store (a, b, c, d) : 'a store) : 'a =
  match p with A -> a | B -> b | C -> c | D -> d

let set (p : t) (x : 'a) (Store (a, b, c, d) : 'a store) : 'a store =
  match p with
  | A -> Store (x, b, c, d)
  | B -> Store (a, x, c, d)
  | C -> Store (a, b, x, d)
  | D -> Store (a, b, c, x)

let find (Store (a, b, c, d) : 'a store) ~(f : 'a -> bool) : t =
  if f a then A
  else if f b then B
  else if f c then C
  else if f d then D
  else failwith "Not found"

type position = Big_master | Small_master | Small_slave | Big_slave
[@@deriving show, eq]

let who_is (pos : position) (store : position store) : t =
  find store ~f:(equal_position pos)

let tests () =
  let store : int store = Store (1, 2, 3, 4) in
  assert (store |> get A = 1);
  assert (store |> set A 5 |> get A = 5);
  Stdio.print_endline "Player: tests passed"
