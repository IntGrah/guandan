open Base

type t =
  | Two
  | Three
  | Four
  | Five
  | Six
  | Seven
  | Eight
  | Nine
  | Ten
  | Jack
  | Queen
  | King
  | Ace
[@@deriving show, eq, ord]

let all : t list =
  [
    Ace; Two; Three; Four; Five; Six; Seven; Eight; Nine; Ten; Jack; Queen; King;
  ]

let consec (x : t) (y : t) : bool =
  match (x, y) with
  | Ace, Two
  | Two, Three
  | Three, Four
  | Four, Five
  | Five, Six
  | Six, Seven
  | Seven, Eight
  | Eight, Nine
  | Nine, Ten
  | Ten, Jack
  | Jack, Queen
  | Queen, King
  | King, Ace ->
      true
  | _ -> false

let ( => ) : t -> t -> bool = consec

let compare_at (level : t) (r0 : t) (r1 : t) : int =
  match (equal r0 level, equal r1 level) with
  | true, true -> 0
  | true, false -> 1
  | false, true -> -1
  | false, false -> compare r0 r1

let tests () =
  assert (compare_at Ten Ten Ace > 0);
  assert (compare_at Nine Ten Ace < 0);
  assert (compare_at Three Three Three = 0);
  assert (compare_at Two King King = 0);
  Stdio.print_endline "Rank: tests passed"
