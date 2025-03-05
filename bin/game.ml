open Base

type t =
  | Winner of Player.team
  | Trading of {
      hands : Card.t list Player.store;
      levels : levels;
      position : Player.position Player.store;
      traded : bool Player.store;
    }
  | Playing of {
      hands : Card.t list Player.store;
      levels : levels;
      finish : finish;
      turn : Player.t;
      current : current;
    }

and levels = Levels of Rank.t * Rank.t * Player.team

and finish =
  | None
  | Big_master of Player.t
  | Small_master of Player.t * Player.t

and current = Lead | Played of Player.t * Score.t [@@deriving show]

type message =
  | Revolt
  | Trade of Card.t list
  | Play of Score.t * Card.t list
  | Pass
[@@deriving show]

type action =
  | None
  | Broadcast of broadcast
  | Private_message of Player.t * string

and broadcast =
  | Passed of Player.t
  | Played of Player.t * Score.t
  | Trade of Player.t * Player.t * Card.t list
[@@deriving show]

let deal_hands () : Card.t list Player.store =
  Card.all @ Card.all
  |> List.map ~f:(fun c -> (Random.bits (), c))
  |> List.sort ~compare:(fun (b0, _) (b1, _) -> Int.compare b0 b1)
  |> List.map ~f:snd |> List.chunks_of ~length:27
  |> function
  | [ ha; hb; hc; hd ] -> Player.Store (ha, hb, hc, hd)
  | _ -> failwith "Should have 108 cards"

let new_game () : t =
  Playing
    {
      hands = deal_hands ();
      levels = Levels (Two, Two, AC);
      finish = None;
      turn = A;
      current = Lead;
    }

let transition (state : t) (player : Player.t) (msg : message) :
    (t * action list, _) Result.t =
  let ( - ) = Multiset.subtract ~equal:Card.equal in

  match state with
  | Winner _ -> Error `Wrong_phase
  | Trading { hands; levels; position; traded } -> (
      let hand = Player.get player hands in
      let level =
        match levels with Levels (r, _, AC) | Levels (_, r, BD) -> r
      in
      let pos = Player.get player position in

      let verify_trade (cards : Card.t list) =
        (* Ensure the correct quantity of cards are being traded,
           and ensure that slaves give their highest cards *)
        match (pos, List.map cards ~f:Rankj.of_card) with
        | Big_master, [ _; _ ] | Small_master, [ _ ] -> true
        | Small_slave, ([ _ ] as offered) | Big_slave, ([ _; _ ] as offered) ->
            let sort_rev =
              Fn.compose List.rev @@ List.sort ~compare:(Rankj.compare_at level)
            in
            let rec compare2 xs ys =
              match (xs, ys) with
              | [], _ | _, [] -> []
              | x :: xs, y :: ys -> Rankj.compare_at level x y :: compare2 xs ys
            in
            hand
            |> List.filter ~f:(Fn.non @@ Card.equal (R (level, Hearts)))
            |> List.map ~f:Rankj.of_card |> sort_rev
            |> compare2 (sort_rev offered)
            |> List.for_all ~f:(fun x -> x >= 0)
        | _ -> false
      in

      match msg with
      | Play _ | Pass -> Error `Wrong_phase
      | Trade _ when Player.get player traded -> Error `Already_traded
      | Revolt -> Error `Not_implemented (* TODO: implement *)
      | Trade cards when not (verify_trade cards) -> Error `Bad_trade
      | Trade cards -> (
          match hand - cards with
          | None -> Error `Not_enough_cards
          | Some hand' -> (
              let receiver : Player.t =
                match pos with
                | Big_master -> Player.who_is Big_slave position
                | Big_slave -> Player.who_is Big_master position
                | Small_master -> Player.who_is Small_slave position
                | Small_slave -> Player.who_is Small_master position
              in
              let hands' =
                hands |> Player.set player hand'
                |> Player.set receiver (Player.get receiver hands @ cards)
              in
              match Player.set player true traded with
              | Store (true, true, true, true) ->
                  Ok
                    ( Playing
                        {
                          hands = hands';
                          levels;
                          finish = None;
                          turn = Player.who_is Big_slave position;
                          current = Lead;
                        },
                      [ Broadcast (Trade (player, receiver, cards)) ] )
              | traded' ->
                  Ok
                    ( Trading
                        { hands = hands'; levels; position; traded = traded' },
                      [ Broadcast (Trade (player, receiver, cards)) ] ))))
  | Playing ({ hands; levels; finish; turn; current } as playing) -> (
      let hand = Player.get player hands in
      let level =
        match levels with Levels (r, _, AC) | Levels (_, r, BD) -> r
      in

      match msg with
      | Revolt | Trade _ -> Error `Wrong_phase
      | (Play _ | Pass) when not (Player.equal player turn) ->
          Error `Not_your_turn
      | Pass -> (
          match current with
          | Lead -> Error `Cannot_pass
          | Played (leader, _) ->
              let rec next_phase (p : Player.t) : t =
                match Player.(get p hands |> List.is_empty, equal p leader) with
                | true, true ->
                    Playing
                      { playing with turn = Player.teammate p; current = Lead }
                | true, false -> next_phase (Player.next p)
                | false, true ->
                    Playing { playing with turn = p; current = Lead }
                | false, false -> Playing { playing with turn = p }
              in
              Ok (next_phase (Player.next turn), [ Broadcast (Passed player) ]))
      | Play (score, cards) when not (Score.verify score cards) ->
          Error `Wrong_score
      | Play (score', cards) -> (
          match (current, hand - cards) with
          | _, None -> Error `Not_enough_cards
          | Played (_, score), _ when not (Score.lt_at level score score') ->
              Error `Doesn't_beat
          | Played (_, _), Some hand' | Lead, Some hand' -> (
              let hands' = Player.set player hand' hands in
              if not (List.is_empty hand') then
                Ok
                  ( Playing
                      {
                        hands = hands';
                        levels;
                        finish;
                        turn = Player.next turn;
                        (* skip players who have finished *)
                        current = Played (player, score');
                      },
                    [ Broadcast (Played (player, score')) ] )
              else
                match finish with
                | None ->
                    Ok
                      ( Playing
                          {
                            hands = hands';
                            levels;
                            finish = Big_master player;
                            turn = Player.next turn;
                            (* skip players who have finished *)
                            current = Played (player, score');
                          },
                        [ Broadcast (Played (player, score')) ] )
                | Big_master bm -> (
                    (* Attempt to short circuit *)
                    match Player.(levels, team bm, team player) with
                    | Levels (Ace, _, AC), AC, AC ->
                        Ok (Winner AC, [ Broadcast (Played (player, score')) ])
                    | Levels (_, Ace, BD), BD, BD ->
                        Ok (Winner BD, [ Broadcast (Played (player, score')) ])
                    | _ ->
                        Ok
                          ( Playing
                              {
                                hands = hands';
                                levels;
                                finish = Small_master (bm, player);
                                turn = Player.next turn;
                                (* skip players who have finished *)
                                current = Played (player, score');
                              },
                            [ Broadcast (Played (player, score')) ] ))
                | Small_master (bm, sm) ->
                    let (Levels (ac, bd, _)) = levels in
                    let levels' : levels =
                      match Player.(team bm, team sm, team player) with
                      | AC, AC, BD ->
                          Levels (Rank.succ @@ Rank.succ @@ Rank.succ ac, bd, AC)
                      | AC, BD, AC -> Levels (Rank.succ @@ Rank.succ ac, bd, AC)
                      | AC, BD, BD -> Levels (Rank.succ ac, bd, AC)
                      | BD, BD, AC ->
                          Levels (ac, Rank.succ @@ Rank.succ @@ Rank.succ bd, BD)
                      | BD, AC, BD -> Levels (ac, Rank.succ @@ Rank.succ bd, BD)
                      | BD, AC, AC -> Levels (ac, Rank.succ bd, BD)
                      | _ -> failwith "Invalid teams"
                    in
                    Ok
                      ( Trading
                          {
                            hands = deal_hands ();
                            levels = levels';
                            position =
                              Player.(
                                init Big_slave |> set bm Big_master
                                |> set sm Small_master |> set player Small_slave);
                            traded = Player.init false;
                          },
                        [ Broadcast (Played (player, score')) ] ))))
