type t = A | B | C | D [@@deriving show, eq]

let next : t -> t = function A -> B | B -> C | C -> D | D -> A
let teammate (p : t) : t = next (next p)

type 'a store = { a : 'a; b : 'a; c : 'a; d : 'a } [@@deriving show]

let get (p : t) (store : 'a store) : 'a =
  match p with A -> store.a | B -> store.b | C -> store.c | D -> store.d

let set (p : t) (v : 'a) (store : 'a store) : 'a store =
  match p with
  | A -> { store with a = v }
  | B -> { store with b = v }
  | C -> { store with c = v }
  | D -> { store with d = v }

let find (v : 'a) { a = va; b = vb; c = vc; d = vd } ~equal : t =
  if equal v va then A
  else if equal v vb then B
  else if equal v vc then C
  else if equal v vd then D
  else failwith "Not found"

let for_all { a = va; b = vb; c = vc; d = vd } ~f : bool =
  f va && f vb && f vc && f vd

let tests () =
  let store = { a = 1; b = 2; c = 3; d = 4 } in
  assert (store |> get A = 1);
  assert (store |> set A 5 |> get A = 5);
  assert (store |> for_all ~f:(fun n -> n > 0));
  Stdio.print_endline "Player: tests passed"

type position = Big_master | Small_master | Small_slave | Big_slave
[@@deriving show, eq]

let who_is (pos : position) (store : position store) =
  find pos store ~equal:equal_position
