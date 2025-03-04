open Base

type t = Active of Card.t list Player.store * levels * phase | Done
[@@deriving show]

and levels = { level : Rank.t; level_13 : Rank.t; level_24 : Rank.t }
[@@deriving show]

and phase =
  | Trading of {
      position : Player.position Player.store;
      traded : bool Player.store;
    }
  | Playing of { turn : Player.t; current : current }
[@@deriving show]

and current = Lead | Resp of Player.t * Score.t [@@deriving show]

type message =
  | Revolt
  | Trade1 of Card.t
  | Trade2 of Card.t * Card.t
  | Play of Score.t * Card.t list
  | Pass
[@@deriving show]

type action =
  | None
  | Broadcast of broadcast
  | PrivateMessage of Player.t * string
[@@deriving show]

and broadcast =
  | Passed of Player.t
  | Played of Player.t * Score.t
  | Trade of Player.t * Player.t * Card.t list
[@@deriving show]

let new_game () : t =
  let deck =
    Card.all @ Card.all
    |> List.map ~f:(fun c -> (Random.bits (), c))
    |> List.sort ~compare:(fun (b0, _) (b1, _) -> Int.compare b0 b1)
    |> List.map ~f:snd
  in

  match List.chunks_of ~length:27 deck with
  | [ ha; hb; hc; hd ] ->
      Active
        ( { a = ha; b = hb; c = hc; d = hd },
          { level = Two; level_13 = Two; level_24 = Two },
          Playing { turn = A; current = Lead } )
  | _ -> failwith "Should have 108 cards"

let transition (state : t) (player : Player.t) (msg : message) :
    (t * action, _) Result.t =
  let ( - ) = Multiset.subtract ~equal:Card.equal in
  match state with
  | Done -> Error `Wrong_phase
  | Active (hands, levels, phase) -> (
      match phase with
      | Trading { position; traded } -> (
          let phase =
            let traded = Player.set player true traded in
            if Player.for_all traded ~f:Fn.id then
              Playing
                { turn = Player.who_is Big_slave position; current = Lead }
            else Trading { position; traded }
          in

          match (msg, Player.get player position) with
          | Play _, _ | Pass, _ -> Error `Wrong_phase
          | (Trade1 _, _ | Trade2 _, _) when Player.get player traded ->
              Error `Already_traded
          | Trade1 _, Big_master
          | Trade1 _, Big_slave
          | Trade2 _, Small_master
          | Trade2 _, Small_slave
          | Revolt, Big_master
          | Revolt, Small_master ->
              Error `Wrong_position
          | Trade1 c, Small_master -> (
              let hand = Player.get player hands in
              let small_slave = Player.who_is Small_slave position in
              let hand_small_slave = Player.get small_slave hands in
              match hand - [ c ] with
              | None -> Error `Not_enough_cards
              | Some hand' ->
                  Ok
                    ( Active
                        ( hands |> Player.set player hand'
                          |> Player.set small_slave (c :: hand_small_slave),
                          levels,
                          phase ),
                      Broadcast (Trade (player, small_slave, [ c ])) ))
          | Trade1 c, Small_slave -> (
              let hand = Player.get player hands in
              let small_master = Player.who_is Small_master position in
              let hand_small_master = Player.get small_master hands in
              match hand - [ c ] with
              | None -> Error `Not_enough_cards
              | Some hand' ->
                  Ok
                    ( Active
                        ( hands |> Player.set player hand'
                          |> Player.set small_master (c :: hand_small_master),
                          levels,
                          phase ),
                      Broadcast (Trade (player, small_master, [ c ])) ))
          | Trade2 (c, c'), Big_master -> (
              let hand = Player.get player hands in
              let big_slave = Player.who_is Big_slave position in
              let hand_big_slave = Player.get big_slave hands in
              match hand - [ c; c' ] with
              | None -> Error `Not_enough_cards
              | Some hand' ->
                  Ok
                    ( Active
                        ( hands |> Player.set player hand'
                          |> Player.set big_slave (c :: c' :: hand_big_slave),
                          levels,
                          phase ),
                      Broadcast (Trade (player, big_slave, [ c; c' ])) ))
          | Trade2 (c, c'), Big_slave -> (
              let hand = Player.get player hands in
              let big_master = Player.who_is Big_master position in
              let hand_big_master = Player.get big_master hands in
              match hand - [ c; c' ] with
              | None -> Error `Not_enough_cards
              | Some hand' ->
                  Ok
                    ( Active
                        ( hands |> Player.set player hand'
                          |> Player.set big_master (c :: c' :: hand_big_master),
                          levels,
                          phase ),
                      Broadcast (Trade (player, big_master, [ c; c' ])) ))
          | Revolt, _ -> Error `Wrong_position (* TODO: implement revolution *))
      | Playing { turn; current } -> (
          match (msg, current) with
          | Revolt, _ | Trade1 _, _ | Trade2 _, _ -> Error `Wrong_phase
          | (Play _, _ | Pass, _) when not (Player.equal player turn) ->
              Error `Not_your_turn
          | Pass, Lead -> Error `Cannot_pass
          | Pass, Resp (leader, _) ->
              let current =
                if Player.(equal (next turn) leader) then Lead else current
              in
              (* TODO: implement follower of leader *)
              let phase = Playing { turn = Player.next turn; current } in
              Ok (Active (hands, levels, phase), Broadcast (Passed player))
          | Play (sc, cs), _ when not (Score.verify sc cs) -> Error `Wrong_score
          | Play (sc, _), Resp (_, sc')
            when not (Score.lt_at levels.level sc' sc) ->
              Error `Doesn't_beat
          | Play (sc, cs), Resp (_, _) | Play (sc, cs), Lead -> (
              let hand = Player.get player hands in
              match hand - cs with
              | None -> Error `Not_enough_cards
              | Some hand ->
                  (* TODO: detect when a playing phase is over *)
                  let phase =
                    Playing
                      { turn = Player.next turn; current = Resp (player, sc) }
                  in
                  Ok
                    ( Active (Player.set player hand hands, levels, phase),
                      Broadcast (Played (player, sc)) ))))
