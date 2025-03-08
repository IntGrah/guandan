open Base
open Guandan.Types

let () =
  let open Card in
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
  assert (compare_at Five (J Black) (J Black) = 0)

let () =
  let open Joker in
  assert (compare Red Red = 0);
  assert (compare Red Black > 0);
  assert (compare Black Red < 0);
  assert (compare Black Black = 0)

let () =
  let open Rank in
  assert (compare_at Ten Ten Ace > 0);
  assert (compare_at Nine Ten Ace < 0);
  assert (compare_at Three Three Three = 0);
  assert (compare_at Two King King = 0)

let () =
  let open Rankj in
  assert (compare_at Ten (R Ten) (R Ace) > 0);
  assert (compare_at Nine (R Ten) (R Ace) < 0);
  assert (compare_at Three (R Three) (R Three) = 0);
  assert (compare_at Two (R King) (R King) = 0);
  assert (compare_at Two (R King) (J Red) < 0);
  assert (compare_at Five (J Black) (J Red) < 0);
  assert (compare_at Five (J Red) (J Black) > 0);
  assert (compare_at Five (J Black) (J Black) = 0)

let () =
  let open Suit in
  assert (compare Diamonds Clubs < 0);
  assert (compare Clubs Hearts < 0);
  assert (compare Hearts Spades < 0);
  assert (compare Spades Hearts > 0);
  assert (compare Hearts Clubs > 0);
  assert (compare Clubs Diamonds > 0);
  assert (compare Spades Spades = 0)
