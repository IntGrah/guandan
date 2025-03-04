open Base

type t = R of Rank.t * Suit.t | J of Joker.t [@@deriving show, eq, ord]

let all : t list =
  J Red :: J Black
  :: List.map (List.cartesian_product Rank.all Suit.all) ~f:(fun (r, s) ->
         R (r, s))

let compare_at (level : Rank.t) (c0 : t) (c1 : t) : int =
  match (c0, c1) with
  | J j0, J j1 -> Joker.compare j0 j1
  | J _, R _ -> 1
  | R _, J _ -> -1
  | R (r0, s0), R (r1, s1) -> (
      match Rank.compare_at level r0 r1 with 0 -> Suit.compare s0 s1 | n -> n)

let tests () =
  assert (List.length all = 54);
  assert (compare_at Ten (R (Ten, Spades)) (R (Ace, Spades)) > 0);
  assert (compare_at Nine (R (Ten, Spades)) (R (Ace, Spades)) < 0);
  assert (compare_at Three (R (Three, Hearts)) (R (Three, Spades)) < 0);
  assert (compare_at Three (R (Three, Hearts)) (R (Three, Hearts)) = 0);
  assert (compare_at Three (R (Three, Hearts)) (R (Three, Clubs)) > 0);
  assert (compare_at Two (R (King, Clubs)) (R (King, Clubs)) = 0);
  assert (compare_at Two (R (King, Diamonds)) (J Red) < 0);
  assert (compare_at Five (J Black) (J Red) < 0);
  assert (compare_at Five (J Red) (J Black) > 0);
  assert (compare_at Five (J Black) (J Black) = 0);
  Stdio.print_endline "Card: tests passed"
