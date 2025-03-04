open Base

type t =
  | JokerBomb (* RRBB *)
  | BombTen of Rank.t (* 2222222222 to AAAAAAAAAA *)
  | BombNine of Rank.t (* 222222222 to AAAAAAAAA *)
  | BombEight of Rank.t (* 22222222 to AAAAAAAA *)
  | BombSeven of Rank.t (* 2222222 to AAAAAAA *)
  | BombSix of Rank.t (* 222222 to AAAAAA *)
  | StraightFlush of Rank.t * Suit.t (* A2345(Diamonds) to TJQKA(Spades) *)
  | BombFive of Rank.t (* 22222 to AAAAA *)
  | BombFour of Rank.t (* 2222 to AAAA *)
  | Diddy2 of Rank.t * Rankj.t * Rankj.t (* AAA222 22 22 to KKKAAA RR BB *)
  | Diddy1 of Rank.t * Rankj.t * Rankj.t (* AAA222 2 2 to KKKAAA R R *)
  | Plate of Rank.t (* AAA222 to KKKAAA *)
  | Tube of Rank.t (* AA2233 to QQKKAA *)
  | Straight of Rank.t (* A2345 to TJQKA *)
  | TripleDouble of Rank.t * Rankj.t (* 222 22 to AAA RR *)
  | TripleSingle of Rank.t * Rankj.t (* 222 2 to AAA R *)
  | Triple of Rank.t (* 222 to AAA *)
  | Pair of Rankj.t (* 22 to RR *)
  | Single of Rankj.t (* 2 to R *)
[@@deriving show]

let verify (score : t) (cards : Card.t list) : bool =
  let rec pairwise (r : 'a -> 'a -> bool) : 'a list -> bool = function
    | [] | [ _ ] -> true
    | x :: x' :: xs -> r x x' && pairwise r (x' :: xs)
  in

  let diddy r r0 r1 =
    Rank.((r0 => r1 && equal r r1) || (r1 => r0 && equal r r0))
  in

  let frequencies : (Rankj.t * int) list =
    cards |> List.map ~f:Rankj.of_card
    |> List.stable_dedup ~compare:Rankj.compare
    |> List.map ~f:(fun k ->
           (k, List.count cards ~f:(fun c -> Rankj.equal k (Rankj.of_card c))))
    |> List.sort ~compare:(fun (k0, f0) (k1, f1) ->
           match Int.compare f0 f1 with 0 -> Rankj.compare k0 k1 | n -> n)
  in

  (* Prolog! *)
  match (frequencies, score) with
  | [ (J Black, 2); (J Red, 2) ], JokerBomb -> true
  | [ (R r, 10) ], BombTen r' -> Rank.equal r r'
  | [ (R r, 9) ], BombNine r' -> Rank.equal r r'
  | [ (R r, 8) ], BombEight r' -> Rank.equal r r'
  | [ (R r, 7) ], BombSeven r' -> Rank.equal r r'
  | [ (R r, 6) ], BombSix r' -> Rank.equal r r'
  | [ (R r, 5) ], BombFive r' -> Rank.equal r r'
  | [ (R r, 4) ], BombFour r' -> Rank.equal r r'
  | ( [ (R r0, 1); (R r1, 1); (R r2, 1); (R r3, 1); (R r4, 1) ],
      StraightFlush (r, s) ) ->
      Rank.(
        (pairwise ( => ) [ r0; r1; r2; r3; r4 ] && equal r r4)
        || (pairwise ( => ) [ r4; r0; r1; r2; r3 ] && equal r r3))
      && List.for_all cards ~f:(function
           | R (_, s') -> Suit.equal s s'
           | _ -> false)
  | [ (R r0, 1); (R r1, 1); (R r2, 1); (R r3, 1); (R r4, 1) ], Straight r ->
      Rank.(
        (pairwise ( => ) [ r0; r1; r2; r3; r4 ] && equal r r4)
        || (pairwise ( => ) [ r4; r0; r1; r2; r3 ] && equal r r3))
  | [ (R r0, 2); (R r1, 2); (R r2, 2) ], Tube r ->
      Rank.(
        (r0 => r1 && r1 => r2 && equal r r2)
        || (r2 => r0 && r0 => r1 && equal r r1))
  | [ (R r0, 3); (R r1, 7) ], Diddy2 (r, R k0, R k1)
  | [ (R r0, 3); (R r1, 5) ], Diddy1 (r, R k0, R k1) ->
      Rank.(
        ((r0 => r1 && equal r r1) || (r1 => r0 && equal r r0))
        && equal r1 k0 && equal r1 k1)
  | [ (R r0, 5); (R r1, 5) ], Diddy2 (r, R k0, R k1)
  | [ (R r0, 4); (R r1, 4) ], Diddy1 (r, R k0, R k1) ->
      diddy r r0 r1
      && Rank.((equal r0 k0 && equal r1 k1) || (equal r1 k0 && equal r0 k1))
  | [ (rj0, 2); (R r0, 3); (R r1, 5) ], Diddy2 (r, kj0, kj1)
  | [ (rj0, 1); (R r0, 3); (R r1, 4) ], Diddy1 (r, kj0, kj1) ->
      diddy r r0 r1
      && Rankj.(
           (equal rj0 kj0 && equal (R r1) kj1)
           || (equal (R r1) kj0 && equal rj0 kj1))
  | [ (rj0, 2); (rj1, 2); (R r0, 3); (R r1, 3) ], Diddy2 (r, kj0, kj1)
  | [ (rj0, 1); (rj1, 1); (R r0, 3); (R r1, 3) ], Diddy1 (r, kj0, kj1) ->
      diddy r r0 r1
      && Rankj.(
           (equal rj0 kj0 && equal rj1 kj1) || (equal rj1 kj0 && equal rj0 kj1))
  | [ (R r0, 3); (R r1, 3) ], Plate r -> diddy r r0 r1
  | [ (R r0, 5) ], TripleDouble (r, R r') | [ (R r0, 4) ], TripleSingle (r, R r')
    ->
      Rank.(equal r0 r && equal r r')
  | [ (rj0, 2); (R r0, 3) ], TripleDouble (r, rj)
  | [ (rj0, 1); (R r0, 3) ], TripleSingle (r, rj) ->
      Rank.equal r r0 && Rankj.equal rj rj0
  | [ (R r0, 3) ], Triple r -> Rank.equal r r0
  | [ (rj0, 2) ], Pair rj | [ (rj0, 1) ], Single rj -> Rankj.equal rj rj0
  | _ -> false

let lt_at (level : Rank.t) (score0 : t) (score1 : t) : bool =
  let ( ||? ) n m = if n = 0 then m else n in
  match (score0, score1) with
  | JokerBomb, _ -> false
  | _, JokerBomb -> true
  | BombTen r0, BombTen r1 -> Rank.compare_at level r0 r1 < 0
  | BombTen _, _ -> false
  | _, BombTen _ -> true
  | BombNine r0, BombNine r1 -> Rank.compare_at level r0 r1 < 0
  | BombNine _, _ -> false
  | _, BombNine _ -> true
  | BombEight r0, BombEight r1 -> Rank.compare_at level r0 r1 < 0
  | BombEight _, _ -> false
  | _, BombEight _ -> true
  | BombSeven r0, BombSeven r1 -> Rank.compare_at level r0 r1 < 0
  | BombSeven _, _ -> false
  | _, BombSeven _ -> true
  | BombSix r0, BombSix r1 -> Rank.compare_at level r0 r1 < 0
  | BombSix _, _ -> false
  | _, BombSix _ -> true
  | StraightFlush (r0, s0), StraightFlush (r1, s1) ->
      Rank.compare r0 r1 ||? Suit.compare s0 s1 < 0
  | StraightFlush _, _ -> false
  | _, StraightFlush _ -> true
  | BombFive r0, BombFive r1 -> Rank.compare_at level r0 r1 < 0
  | BombFive _, _ -> false
  | _, BombFive _ -> true
  | BombFour r0, BombFour r1 -> Rank.compare_at level r0 r1 < 0
  | BombFour _, _ -> false
  | _, BombFour _ -> true
  | Diddy2 (r0, rj0, rj0'), Diddy2 (r1, rj1, rj1') ->
      Rank.compare r0 r1
      ||? Rankj.compare_at level rj0 rj1
      ||? Rankj.compare_at level rj0' rj1'
      < 0
  | Diddy1 (r0, rj0, rj0'), Diddy1 (r1, rj1, rj1') ->
      Rank.compare r0 r1
      ||? Rankj.compare_at level rj0 rj1
      ||? Rankj.compare_at level rj0' rj1'
      < 0
  | Plate r0, Plate r1 | Tube r0, Tube r1 | Straight r0, Straight r1 ->
      Rank.compare r0 r1 < 0
  | TripleDouble (r0, rj0), TripleDouble (r1, rj1)
  | TripleSingle (r0, rj0), TripleSingle (r1, rj1) ->
      Rank.compare_at level r0 r1 ||? Rankj.compare_at level rj0 rj1 < 0
  | Triple r0, Triple r1 -> Rank.compare_at level r0 r1 < 0
  | Pair rj0, Pair rj1 | Single rj0, Single rj1 ->
      Rankj.compare_at level rj0 rj1 < 0
  | _ -> false

let tests () =
  let assert_score score cards = assert (verify score cards) in
  let assert_not_score score cards = assert (not @@ verify score cards) in
  assert_score JokerBomb [ J Red; J Black; J Black; J Red ];
  assert_score (BombTen Ace)
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
    ];
  assert_score (BombFour Four)
    [ R (Four, Hearts); R (Four, Clubs); R (Four, Spades); R (Four, Diamonds) ];
  assert_score
    (TripleSingle (Four, J Red))
    [ R (Four, Hearts); J Red; R (Four, Clubs); R (Four, Spades) ];
  assert_not_score
    (TripleDouble (Three, R Two))
    [
      R (Five, Hearts);
      R (Three, Clubs);
      R (Five, Clubs);
      R (Two, Spades);
      R (Five, Spades);
    ];
  assert_score
    (TripleDouble (Four, J Red))
    [ R (Four, Hearts); J Red; R (Four, Clubs); J Red; R (Four, Spades) ];
  assert_score
    (StraightFlush (Five, Clubs))
    [
      R (Five, Clubs);
      R (Four, Clubs);
      R (Three, Clubs);
      R (Ace, Clubs);
      R (Two, Clubs);
    ];
  assert_score
    (StraightFlush (Ace, Clubs))
    [
      R (Ten, Clubs);
      R (Jack, Clubs);
      R (Queen, Clubs);
      R (King, Clubs);
      R (Ace, Clubs);
    ];
  assert_score (Straight Five)
    [
      R (Ace, Clubs);
      R (Two, Clubs);
      R (Three, Clubs);
      R (Four, Diamonds);
      R (Five, Clubs);
    ];
  assert_score (Straight Ace)
    [
      R (Ten, Clubs);
      R (Jack, Clubs);
      R (Queen, Diamonds);
      R (King, Clubs);
      R (Ace, Clubs);
    ];
  assert_score (Plate Two)
    [
      R (Ace, Clubs);
      R (Two, Clubs);
      R (Two, Clubs);
      R (Ace, Clubs);
      R (Two, Spades);
      R (Ace, Spades);
    ];
  assert_score (Plate Ace)
    [
      R (Ace, Clubs);
      R (King, Clubs);
      R (King, Clubs);
      R (Ace, Clubs);
      R (King, Spades);
      R (Ace, Spades);
    ];
  assert_score
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
    ];
  assert_score
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
    ];
  assert_score
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
    ];
  assert_score (Triple Ten) [ R (Ten, Clubs); R (Ten, Clubs); R (Ten, Spades) ];
  assert_score (Pair (R Ace)) [ R (Ace, Clubs); R (Ace, Hearts) ];
  assert_score (Pair (J Red)) [ J Red; J Red ];
  assert_not_score (Pair (J Red)) [ J Red; J Black ];
  assert (lt_at Ten (Diddy1 (Ace, R Two, R Three)) (Diddy1 (Ace, R Two, R Four)));
  assert (lt_at Ten (TripleDouble (Ace, J Red)) (TripleDouble (Ten, R Five)));
  assert (lt_at Nine (TripleDouble (Ten, R Five)) (TripleDouble (Ace, J Red)));
  Stdio.print_endline "Score: tests passed"
