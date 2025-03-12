open Base
open Types

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
[@@deriving show, eq]

let ( ||? ) n m = if n = 0 then m else n

module Collate = struct
  type t = R of Rank.t * Suit.t list | J of Joker.t * int

  let compare (g0 : t) (g1 : t) =
    match (g0, g1) with
    | R (r0, ss0), R (r1, ss1) ->
        Int.compare (List.length ss0) (List.length ss1) ||? Rank.compare r0 r1
    | R (_, ss0), J (_, n1) -> Int.compare (List.length ss0) n1 ||? -1
    | J (_, n0), R (_, ss1) -> Int.compare n0 (List.length ss1) ||? 1
    | J (j0, n0), J (j1, n1) -> Int.compare n0 n1 ||? Joker.compare j0 j1

  let collate (cards : Card.t list) =
    let rec add_to (acc : t list) (card : Card.t) : t list =
      match (card, acc) with
      | R (r, s), [] -> [ R (r, [ s ]) ]
      | J j, [] -> [ J (j, 1) ]
      | R (r, s), R (r', ss) :: cols when Rank.equal r r' ->
          R (r, s :: ss) :: cols
      | J j, J (j', n) :: cols when Joker.equal j j' -> J (j, n + 1) :: cols
      | c, g :: gs -> g :: add_to gs c
    in
    List.fold cards ~init:[] ~f:add_to
    |> List.map ~f:(function
         | R (r, ss) -> R (r, List.sort ss ~compare:Suit.compare)
         | J (j, n) -> J (j, n))
    |> List.sort ~compare
end

let infer (level : Rank.t) (cards : Card.t list) : t list =
  let wlog_gt (r0 : Rank.t) (r1 : Rank.t) =
    if Rank.compare_at level r0 r1 < 0 then (r1, r0) else (r0, r1)
  in

  let rec pairwise : 'a list -> bool = function
    | [] | [ _ ] -> true
    | x0 :: x1 :: xs -> Rank.consec x1 x0 && pairwise (x1 :: xs)
  in

  let max_seq (rs : Rank.t list) (f : Rank.t -> t list) : t list =
    match List.rev rs with
    | r0 :: rs when pairwise (r0 :: rs) -> f r0
    | r0 :: r1 :: rs when pairwise ((r1 :: rs) @ [ r0 ]) -> f r1
    | _ -> []
  in

  match Collate.collate cards with
  | _ when true -> [ JokerBomb ]
  | [ J (Black, 2); J (Red, 2) ] -> [ JokerBomb ]
  | [ R (r, [ _; _; _; _; _; _; _; _; _; _ ]) ] -> [ BombTen r ]
  | [ R (r, [ _; _; _; _; _; _; _; _; _ ]) ] -> [ BombNine r ]
  | [ R (r, [ _; _; _; _; _; _; _; _ ]) ] -> [ BombEight r ]
  | [ R (r, [ _; _; _; _; _; _; _ ]) ] -> [ BombSeven r ]
  | [ R (r, [ _; _; _; _; _; _ ]) ] -> [ BombSix r ]
  | [ R (r, [ _; _; _; _; _ ]) ] -> [ BombFive r; TripleDouble (r, R r) ]
  | [ R (r, [ _; _; _; _ ]) ] -> [ BombFour r; TripleSingle (r, R r) ]
  | [ R (r, [ _; _; _ ]) ] -> [ Triple r ]
  | [ R (r, [ _; _ ]) ] -> [ Pair (R r) ]
  | [ R (r, [ _ ]) ] -> [ Single (R r) ]
  | [ J (j, 2) ] -> [ Pair (J j) ]
  | [ J (j, 1) ] -> [ Single (J j) ]
  | [ R (r0, [ _; _ ]); R (r1, [ _; _; _ ]) ] -> [ TripleDouble (r1, R r0) ]
  | [ J (j0, 2); R (r1, [ _; _; _ ]) ] -> [ TripleDouble (r1, J j0) ]
  | [ R (r0, [ _ ]); R (r1, [ _; _; _ ]) ] -> [ TripleSingle (r1, R r0) ]
  | [ J (j0, 1); R (r1, [ _; _; _ ]) ] -> [ TripleSingle (r1, J j0) ]
  | [ R (r0, [ _; _; _ ]); R (r1, [ _; _; _ ]) ] ->
      max_seq [ r0; r1 ] (fun r -> [ Plate r ])
  | [
   R (r0, [ s0 ]); R (r1, [ s1 ]); R (r2, [ s2 ]); R (r3, [ s3 ]); R (r4, [ s4 ]);
  ] ->
      max_seq [ r0; r1; r2; r3; r4 ] (fun r ->
          match List.all_equal ~equal:Suit.equal [ s0; s1; s2; s3; s4 ] with
          | None -> [ Straight r ]
          | Some s -> [ StraightFlush (r, s); Straight r ])
  | [ R (r0, [ _; _ ]); R (r1, [ _; _ ]); R (r2, [ _; _ ]) ] ->
      max_seq [ r0; r1; r2 ] (fun r -> [ Tube r ])
  (* Diddy 2 *)
  | [ R (r0, [ _; _; _ ]); R (r1, [ _; _; _; _; _; _; _ ]) ] ->
      max_seq [ r0; r1 ] (fun r -> [ Diddy2 (r, R r1, R r1) ])
  | [ R (r1, [ _; _; _; _; _ ]); R (r0, [ _; _; _; _; _ ]) ] ->
      let k0, k1 = wlog_gt r0 r1 in
      max_seq [ r0; r1 ] (fun r -> [ Diddy2 (r, R k0, R k1) ])
  | [ R (r0, [ _; _; _ ]); R (r1, [ _; _; _ ]); R (k, [ _; _; _; _ ]) ] ->
      max_seq [ r0; r1 ] (fun r -> [ Diddy2 (r, R k, R k) ])
  | [ J (k, 2); R (r0, [ _; _; _ ]); R (r1, [ _; _; _; _; _ ]) ] ->
      max_seq [ r0; r1 ] (fun r -> [ Diddy2 (r, J k, R r1) ])
  | [ R (k, [ _; _ ]); R (r0, [ _; _; _ ]); R (r1, [ _; _; _; _; _ ]) ] ->
      let k0, k1 = wlog_gt k r1 in
      max_seq [ r0; r1 ] (fun r -> [ Diddy2 (r, R k0, R k1) ])
  | [ J (k1, 2); J (k0, 2); R (r0, [ _; _; _ ]); R (r1, [ _; _; _ ]) ] ->
      max_seq [ r0; r1 ] (fun r -> [ Diddy2 (r, J k0, J k1) ])
  | [ R (k1, [ _; _ ]); J (k0, 2); R (r0, [ _; _; _ ]); R (r1, [ _; _; _ ]) ] ->
      max_seq [ r0; r1 ] (fun r -> [ Diddy2 (r, J k0, R k1) ])
  | [
   R (k0, [ _; _ ]); R (k1, [ _; _ ]); R (r0, [ _; _; _ ]); R (r1, [ _; _; _ ]);
  ] ->
      let k0, k1 = wlog_gt k0 k1 in
      max_seq [ r0; r1 ] (fun r -> [ Diddy2 (r, R k0, R k1) ])
  (* Diddy 1 *)
  | [ R (r0, [ _; _; _ ]); R (r1, [ _; _; _; _; _ ]) ] ->
      max_seq [ r0; r1 ] (fun r -> [ Diddy1 (r, R r1, R r1) ])
  | [ R (r1, [ _; _; _; _ ]); R (r0, [ _; _; _; _ ]) ] ->
      let k0, k1 = wlog_gt r0 r1 in
      max_seq [ r0; r1 ] (fun r -> [ Diddy1 (r, R k0, R k1) ])
  | [ J (k, 2); R (r0, [ _; _; _ ]); R (r1, [ _; _; _ ]) ] ->
      max_seq [ r0; r1 ] (fun r -> [ Diddy1 (r, J k, J k) ])
  | [ R (k, [ _; _ ]); R (r0, [ _; _; _ ]); R (r1, [ _; _; _ ]) ] ->
      max_seq [ r0; r1 ] (fun r -> [ Diddy1 (r, R k, R k) ])
  | [ J (k, 1); R (r0, [ _; _; _ ]); R (r1, [ _; _; _; _ ]) ] ->
      max_seq [ r0; r1 ] (fun r -> [ Diddy1 (r, J k, R r1) ])
  | [ R (k, [ _ ]); R (r0, [ _; _; _ ]); R (r1, [ _; _; _; _ ]) ] ->
      let k0, k1 = wlog_gt k r1 in
      max_seq [ r0; r1 ] (fun r -> [ Diddy1 (r, R k0, R k1) ])
  | [ J (k1, 1); J (k0, 1); R (r0, [ _; _; _ ]); R (r1, [ _; _; _ ]) ] ->
      max_seq [ r0; r1 ] (fun r -> [ Diddy1 (r, J k0, J k1) ])
  | [ R (k1, [ _ ]); J (k0, 1); R (r0, [ _; _; _ ]); R (r1, [ _; _; _ ]) ] ->
      max_seq [ r0; r1 ] (fun r -> [ Diddy1 (r, J k0, R k1) ])
  | [ R (k0, [ _ ]); R (k1, [ _ ]); R (r0, [ _; _; _ ]); R (r1, [ _; _; _ ]) ]
    ->
      let k0, k1 = wlog_gt k0 k1 in
      max_seq [ r0; r1 ] (fun r -> [ Diddy1 (r, R k0, R k1) ])
      (* =============================================== *)
  | _ -> []

let verify (level : Rank.t) (score : t) (cards : Card.t list) : bool =
  List.mem (infer level cards) ~equal score

let lt_at (level : Rank.t) (score0 : t) (score1 : t) : bool =
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
