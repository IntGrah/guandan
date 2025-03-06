open Base

type t =
  | Winner of Player.team
  | Trading of {
      hands : Card.t list Player.store;
      levels : level;
      position : Player.position Player.store;
      traded : bool Player.store;
    }
  | Playing of {
      hands : Card.t list Player.store;
      levels : level;
      finished : finished;
      turn : Player.t;
      current : (Player.t * Score.t) option;
    }

and level = { ac : Rank.t; bd : Rank.t; which : Player.team }

and finished =
  | No_one
  | Big_master of Player.t
  | Small_master of Player.t * Player.t
[@@deriving show]

let rank_of = function
  | { ac = r; bd = _; which = AC } | { ac = _; bd = r; which = BD } -> r

type message =
  | Revolt
  | Trade of Card.t list
  | Play of Score.t * Card.t list
  | Pass
[@@deriving show]

type 'b action = None | Broadcast of 'b | Private_message of Player.t * string
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
      levels = { ac = Two; bd = Two; which = AC };
      finished = No_one;
      turn = A;
      current = None;
    }

let transition (state : t) (player : Player.t) (msg : message) :
    (t * _ action list, _) Result.t =
  let ( - ) (xs : Card.t list) (ys : Card.t list) =
    Multiset.subtract xs ys ~equal:Card.equal
    |> Result.of_option ~error:`Not_enough_cards
  in

  let ( let+ ) x f = Result.bind x ~f in

  match (state, msg) with
  | Trading { traded; _ }, Trade _ when Player.get player traded ->
      Error `Already_traded
  | Trading _, Revolt -> Error `Not_implemented (* TODO: implement *)
  | Trading { hands; levels; position; traded }, Trade cards -> (
      let hand = Player.get player hands in
      let+ () =
        let level = rank_of levels in
        (* Ensure the correct quantity of cards are being traded,
     and ensure that slaves give their highest cards *)
        match (Player.get player position, List.map cards ~f:Rankj.of_card) with
        | Big_master, [ _; _ ] | Small_master, [ _ ] -> Ok ()
        | Small_slave, ([ _ ] as offered) | Big_slave, ([ _; _ ] as offered) ->
            let sort_rev =
              Fn.compose List.rev @@ List.sort ~compare:(Rankj.compare_at level)
            in
            let rec compare2 xs ys =
              match (xs, ys) with
              | [], _ | _, [] -> []
              | x :: xs, y :: ys -> Rankj.compare_at level x y :: compare2 xs ys
            in
            if
              hand
              |> List.filter ~f:(Fn.non @@ Card.equal (R (level, Hearts)))
              |> List.map ~f:Rankj.of_card |> sort_rev
              |> compare2 (sort_rev offered)
              |> List.for_all ~f:(fun x -> x >= 0)
            then Ok ()
            else Error `Must_give_highest_cards
        | _ -> Error `Incorrent_number_of_cards
      in

      let+ hand' = hand - cards in
      let receiver : Player.t =
        match Player.get player position with
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
                  finished = No_one;
                  turn = Player.who_is Big_slave position;
                  current = None;
                },
              [ Broadcast (`Trade (player, receiver, cards)) ] )
      | traded' ->
          Ok
            ( Trading { hands = hands'; levels; position; traded = traded' },
              [ Broadcast (`Trade (player, receiver, cards)) ] ))
  | Playing { turn; _ }, (Play _ | Pass) when not (Player.equal player turn) ->
      Error `Not_your_turn
  | Playing { hands; levels; finished; turn; current }, Pass ->
      let+ leader, _ = Result.of_option current ~error:`Cannot_pass in
      let rec next_state (turn : Player.t) : t =
        let turn' = Player.next turn in
        match Player.(get turn' hands |> List.is_empty, equal turn' leader) with
        | true, true ->
            let turn'' = Player.teammate turn' in
            Playing { hands; levels; finished; turn = turn''; current = None }
        | true, false -> next_state turn'
        | false, true ->
            Playing { hands; levels; finished; turn = turn'; current = None }
        | false, false ->
            Playing { hands; levels; finished; turn = turn'; current }
      in

      Ok (next_state turn, [ Broadcast (`Passed player) ])
  | Playing { levels; _ }, Play (score', cards')
    when not (Score.verify (rank_of levels) score' cards') ->
      Error `Wrong_score
  | Playing { hands; levels; finished; turn; current }, Play (score', cards)
    -> (
      let+ hand' = Player.get player hands - cards in
      let+ () =
        match current with
        | Some (_, score) when not (Score.lt_at (rank_of levels) score score')
          ->
            Error `Doesn't_beat
        | _ -> Ok ()
      in
      let hands' = Player.set player hand' hands in
      let turn' =
        Player.find_next turn ~f:(fun p ->
            Player.get p hands |> Fn.non List.is_empty)
      in
      let current' = Some (player, score') in
      match (hand', finished) with
      | _ :: _, _ ->
          Ok
            ( Playing
                {
                  hands = hands';
                  levels;
                  finished;
                  turn = turn';
                  current = current';
                },
              [ Broadcast (`Played (player, score')) ] )
      | [], No_one ->
          Ok
            ( Playing
                {
                  hands = hands';
                  levels;
                  finished = Big_master player;
                  turn = turn';
                  current = current';
                },
              [ Broadcast (`Played (player, score')) ] )
      | [], Big_master bm -> (
          (* Attempt to short circuit *)
          match Player.(levels, team bm, team player) with
          | { ac = Ace; bd = _; which = AC }, AC, AC ->
              Ok
                ( Winner AC,
                  [
                    Broadcast (`Played (player, score'));
                    Broadcast (`Won Player.AC);
                  ] )
          | { ac = _; bd = Ace; which = BD }, BD, BD ->
              Ok
                ( Winner BD,
                  [
                    Broadcast (`Played (player, score'));
                    Broadcast (`Won Player.BD);
                  ] )
          | _ ->
              Ok
                ( Playing
                    {
                      hands = hands';
                      levels;
                      finished = Small_master (bm, player);
                      turn = turn';
                      current = current';
                    },
                  [ Broadcast (`Played (player, score')) ] ))
      | [], Small_master (bm, sm) ->
          let { ac; bd; _ } = levels in
          let levels' : level =
            Rank.(
              match Player.(team bm, team sm, team player) with
              | AC, AC, BD -> { bd; which = AC; ac = succ @@ succ @@ succ ac }
              | AC, BD, AC -> { bd; which = AC; ac = succ @@ succ ac }
              | AC, BD, BD -> { bd; which = AC; ac = succ ac }
              | BD, BD, AC -> { ac; which = BD; bd = succ @@ succ @@ succ bd }
              | BD, AC, BD -> { ac; which = BD; bd = succ @@ succ bd }
              | BD, AC, AC -> { ac; which = BD; bd = succ bd }
              | _ -> failwith "Invalid teams")
          in
          let position' =
            Player.(
              init Big_slave |> set bm Big_master |> set sm Small_master
              |> set player Small_slave)
          in
          Ok
            ( Trading
                {
                  hands = deal_hands ();
                  levels = levels';
                  position = position';
                  traded = Player.init false;
                },
              [
                Broadcast (`Played (player, score'));
                Broadcast (`Round_over position');
              ] ))
  | Winner _, _
  | Trading _, Play _
  | Trading _, Pass
  | Playing _, Revolt
  | Playing _, Trade _ ->
      Error `Wrong_phase
