open Base

type t = R of Rank.t | J of Joker.t [@@deriving show, eq, ord]

let of_card : Card.t -> t = function R (r, _) -> R r | J j -> J j

let compare_at (level : Rank.t) (rj0 : t) (rj1 : t) : int =
  match (rj0, rj1) with
  | J j0, J j1 -> Joker.compare j0 j1
  | J _, R _ -> 1
  | R _, J _ -> -1
  | R r0, R r1 -> Rank.compare_at level r0 r1

let tests () =
  assert (compare_at Ten (R Ten) (R Ace) > 0);
  assert (compare_at Nine (R Ten) (R Ace) < 0);
  assert (compare_at Three (R Three) (R Three) = 0);
  assert (compare_at Two (R King) (R King) = 0);
  assert (compare_at Two (R King) (J Red) < 0);
  assert (compare_at Five (J Black) (J Red) < 0);
  assert (compare_at Five (J Red) (J Black) > 0);
  assert (compare_at Five (J Black) (J Black) = 0);
  Stdio.print_endline "Rankj: tests passed"
