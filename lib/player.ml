open Base

type t = A | B | C | D [@@deriving show, eq]
type team = AC | BD [@@deriving show]

let next : t -> t = function A -> B | B -> C | C -> D | D -> A
let teammate : t -> t = Fn.compose next next
let team : t -> team = function A | C -> AC | B | D -> BD

type 'a store = Store of 'a * 'a * 'a * 'a [@@deriving show]

let get (p : t) (Store (a, b, c, d) : 'a store) : 'a =
  match p with A -> a | B -> b | C -> c | D -> d

let init (x : 'a) : 'a store = Store (x, x, x, x)

let set (player : t) (x : 'a) (Store (a, b, c, d) : 'a store) : 'a store =
  match player with
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

let who_is (position : position) (store : position store) : t =
  find store ~f:(equal_position position)
