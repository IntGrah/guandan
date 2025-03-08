open Base
open Guandan.Score

let assert_score level score cards = assert (verify level score cards)

let assert_not_score level score cards =
  assert (verify level score cards |> not)

let () = assert_score Three JokerBomb [ J Red; J Black; J Black; J Red ]

let () =
  assert_score Three (BombTen Ace)
    [
      R (Ace, Clubs);
      R (Ace, Clubs);
      R (Ace, Spades);
      R (Ace, Spades);
      R (Ace, Diamonds);
      R (Ace, Diamonds);
      R (Ace, Hearts);
      R (Ace, Hearts);
      R (Ace, Hearts);
      R (Ace, Hearts);
    ]

let () =
  assert_score Three (BombFour Four)
    [ R (Four, Hearts); R (Four, Clubs); R (Four, Spades); R (Four, Diamonds) ]

let () =
  assert_score Three
    (TripleSingle (Four, J Red))
    [ R (Four, Hearts); J Red; R (Four, Clubs); R (Four, Spades) ]

let () =
  assert_not_score Three
    (TripleDouble (Three, R Two))
    [
      R (Five, Hearts);
      R (Three, Clubs);
      R (Five, Clubs);
      R (Two, Spades);
      R (Five, Spades);
    ]

let () =
  assert_score Three
    (TripleDouble (Four, J Red))
    [ R (Four, Hearts); J Red; R (Four, Clubs); J Red; R (Four, Spades) ]

let () =
  assert_score Three
    (StraightFlush (Five, Clubs))
    [
      R (Five, Clubs);
      R (Four, Clubs);
      R (Three, Clubs);
      R (Ace, Clubs);
      R (Two, Clubs);
    ]

let () =
  assert_score Three
    (StraightFlush (Ace, Clubs))
    [
      R (Ten, Clubs);
      R (Jack, Clubs);
      R (Queen, Clubs);
      R (King, Clubs);
      R (Ace, Clubs);
    ]

let () =
  assert_score Three (Straight Five)
    [
      R (Ace, Clubs);
      R (Two, Clubs);
      R (Three, Clubs);
      R (Four, Diamonds);
      R (Five, Clubs);
    ]

let () =
  assert_score Three (Straight Ace)
    [
      R (Ten, Clubs);
      R (Jack, Clubs);
      R (Queen, Diamonds);
      R (King, Clubs);
      R (Ace, Clubs);
    ]

let () =
  assert_score Three (Plate Two)
    [
      R (Ace, Clubs);
      R (Two, Clubs);
      R (Two, Clubs);
      R (Ace, Clubs);
      R (Two, Spades);
      R (Ace, Spades);
    ]

let () =
  assert_score Three (Plate Ace)
    [
      R (Ace, Clubs);
      R (King, Clubs);
      R (King, Clubs);
      R (Ace, Clubs);
      R (King, Spades);
      R (Ace, Spades);
    ]

let () =
  assert_score Three
    (Diddy2 (Two, J Red, J Black))
    [
      J Black;
      J Black;
      J Red;
      J Red;
      R (Ace, Clubs);
      R (Two, Clubs);
      R (Two, Clubs);
      R (Ace, Clubs);
      R (Two, Clubs);
      R (Ace, Clubs);
    ]

let () =
  assert_not_score Three
    (Diddy2 (Two, J Black, J Red))
    [
      J Black;
      J Black;
      J Red;
      J Red;
      R (Ace, Clubs);
      R (Two, Clubs);
      R (Two, Clubs);
      R (Ace, Clubs);
      R (Two, Clubs);
      R (Ace, Clubs);
    ]

let () =
  assert_score Three
    (Diddy2 (Ace, R King, R Queen))
    [
      R (King, Clubs);
      R (King, Clubs);
      R (Queen, Clubs);
      R (Queen, Clubs);
      R (Ace, Clubs);
      R (King, Clubs);
      R (King, Clubs);
      R (Ace, Clubs);
      R (King, Clubs);
      R (Ace, Clubs);
    ]

let () =
  assert_not_score Queen
    (Diddy2 (Ace, R King, R Queen))
    [
      R (King, Clubs);
      R (King, Clubs);
      R (Queen, Clubs);
      R (Queen, Clubs);
      R (Ace, Clubs);
      R (King, Clubs);
      R (King, Clubs);
      R (Ace, Clubs);
      R (King, Clubs);
      R (Ace, Clubs);
    ]

let () =
  assert_score Queen
    (Diddy2 (Ace, R Queen, R King))
    [
      R (King, Clubs);
      R (King, Clubs);
      R (Queen, Clubs);
      R (Queen, Clubs);
      R (Ace, Clubs);
      R (King, Clubs);
      R (King, Clubs);
      R (Ace, Clubs);
      R (King, Clubs);
      R (Ace, Clubs);
    ]

let () =
  assert_score Three (Triple Ten)
    [ R (Ten, Clubs); R (Ten, Clubs); R (Ten, Spades) ]

let () = assert_score Three (Pair (R Ace)) [ R (Ace, Clubs); R (Ace, Hearts) ]
let () = assert_score Three (Pair (J Red)) [ J Red; J Red ]
let () = assert_not_score Three (Pair (J Red)) [ J Red; J Black ]

let () =
  assert (lt_at Ten (Diddy1 (Ace, R Two, R Three)) (Diddy1 (Ace, R Two, R Four)))

let () =
  assert (lt_at Five (Diddy1 (Ace, R Two, R Six)) (Diddy1 (Ace, R Two, R Five)))

let () =
  assert (lt_at Six (Diddy1 (Ace, R Seven, R Six)) (Diddy1 (Ace, R Six, R Five)))

let () =
  assert (lt_at Ten (TripleDouble (Ace, J Red)) (TripleDouble (Ten, R Five)))

let () =
  assert (lt_at Nine (TripleDouble (Ten, R Five)) (TripleDouble (Ace, J Red)))
