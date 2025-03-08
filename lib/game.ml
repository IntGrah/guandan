open Base
open Types

type t =
  | Winner of level
  | Trading of
      Card.t list Player.store
      * level
      * Player.position Player.store
      * bool Player.store
  | Playing of
      Card.t list Player.store
      * level
      * finished
      * Player.t
      * (Player.t * Score.t) option

and level = Level of Rank.t * Rank.t * Player.team

and finished =
  | No_one
  | Big_master of Player.t
  | Small_master of Player.t * Player.t
[@@deriving show]

let rank_of : level -> Rank.t = function
  | Level (r, _, AC) | Level (_, r, BD) -> r

type message =
  | Revolt
  | Trade of Card.t list
  | Play of Score.t * Card.t list
  | Pass
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
  Playing (deal_hands (), Level (Two, Two, AC), No_one, A, None)

let transition (state : t) (player : Player.t) (msg : message) :
    (t * _ list, _) Result.t =
  let ( let* ) x f = Result.bind x ~f in
  let ( - ) (xs : Card.t list) (ys : Card.t list) =
    Multiset.subtract xs ys ~equal:Card.equal
    |> Result.of_option ~error:`Not_enough_cards
  in

  match (state, msg) with
  | Trading (_, _, _, traded), Trade _ when Player.get player traded ->
      Error `Already_traded
  | Trading _, Revolt -> Error `Not_implemented (* TODO: implement *)
  | Trading (hands, level, position, traded), Trade cards -> (
      let hand = Player.get player hands in
      let* () =
        let level = rank_of level in
        (* Ensure the correct quantity of cards are being traded,
           and ensure that slaves give their highest cards *)
        match (Player.get player position, List.map cards ~f:Rankj.of_card) with
        | Big_master, [ _; _ ] | Small_master, [ _ ] -> Ok ()
        | Small_slave, ([ _ ] as offered) | Big_slave, ([ _; _ ] as offered) ->
            let sort_rev l =
              List.rev @@ List.sort l ~compare:(Rankj.compare_at level)
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

      let receiver : Player.t =
        match Player.get player position with
        | Big_master -> Player.who_is Big_slave position
        | Big_slave -> Player.who_is Big_master position
        | Small_master -> Player.who_is Small_slave position
        | Small_slave -> Player.who_is Small_master position
      in
      let* hand' = hand - cards in
      let hands' =
        hands |> Player.set player hand'
        |> Player.set receiver (Player.get receiver hands @ cards)
      in
      match Player.set player true traded with
      | Store (true, true, true, true) ->
          Ok
            ( Playing
                (hands', level, No_one, Player.who_is Big_slave position, None),
              [ `Trade (player, receiver, cards) ] )
      | traded' ->
          Ok
            ( Trading (hands', level, position, traded'),
              [ `Trade (player, receiver, cards) ] ))
  | Playing (_, _, _, turn, _), (Play _ | Pass)
    when not (Player.equal player turn) ->
      Error `Not_your_turn
  | Playing (hands, level, finished, turn, current), Pass ->
      let* leader, _ = Result.of_option current ~error:`Cannot_pass in
      let rec next_state (turn : Player.t) : t =
        let turn' = Player.next turn in
        match Player.(get turn' hands |> List.is_empty, equal turn' leader) with
        | true, true ->
            let turn'' = Player.teammate turn' in
            Playing (hands, level, finished, turn'', None)
        | true, false -> next_state turn'
        | false, true -> Playing (hands, level, finished, turn', None)
        | false, false -> Playing (hands, level, finished, turn', current)
      in

      Ok (next_state turn, [ `Passed player ])
  | Playing (hands, level, finished, turn, current), Play (score', cards') -> (
      let* () =
        if Score.verify (rank_of level) score' cards' then Ok ()
        else Error `Wrong_score
      in
      let* () =
        match current with
        | Some (_, score) when not (Score.lt_at (rank_of level) score score') ->
            Error `Doesn't_beat
        | _ -> Ok ()
      in
      let* hand' = Player.get player hands - cards' in
      let hands' = Player.set player hand' hands in

      let rec find_next (player : Player.t) : Player.t =
        let next_player = Player.next player in
        if Player.get next_player hands |> Fn.non List.is_empty then next_player
        else find_next next_player
      in
      let continue (finished' : finished) =
        Ok
          ( Playing
              (hands', level, finished', find_next turn, Some (player, score')),
            [ `Played (player, score') ] )
      in
      match (hand', finished) with
      | _ :: _, _ -> continue finished
      | [], No_one -> continue (Big_master player)
      | [], Big_master bm -> (
          (* Attempt to short circuit *)
          match Player.(level, team bm, team player) with
          | Level (Ace, _, AC), AC, AC ->
              Ok (Winner level, [ `Played (player, score'); `Won Player.AC ])
          | Level (_, Ace, BD), BD, BD ->
              Ok (Winner level, [ `Played (player, score'); `Won Player.BD ])
          | _ -> continue (Small_master (bm, player)))
      | [], Small_master (bm, sm) ->
          let (Level (ac, bd, _)) = level in
          let level' : level =
            Rank.(
              match Player.(team bm, team sm, team player) with
              | AC, AC, BD -> Level (ac |> succ |> succ |> succ, bd, AC)
              | AC, BD, AC -> Level (ac |> succ |> succ, bd, AC)
              | AC, BD, BD -> Level (ac |> succ, bd, AC)
              | BD, BD, AC -> Level (ac, bd |> succ |> succ |> succ, BD)
              | BD, AC, BD -> Level (ac, bd |> succ |> succ, BD)
              | BD, AC, AC -> Level (ac, bd |> succ, BD)
              | _ -> failwith "Invalid teams")
          in
          let position' =
            Player.(
              init Big_slave |> set bm Big_master |> set sm Small_master
              |> set player Small_slave)
          in
          Ok
            ( Trading (deal_hands (), level', position', Player.init false),
              [ `Played (player, score'); `New_round position' ] ))
  | Winner _, _
  | Trading _, Play _
  | Trading _, Pass
  | Playing _, Revolt
  | Playing _, Trade _ ->
      Error `Wrong_phase
