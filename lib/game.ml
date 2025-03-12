open Base
open Types

type t =
  | Winner of level
  | Trading of
      Card.t list Player.store
      * level
      * Player.position Player.store
      * int Player.store
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
  | Sort
  | Revolt
  | Trade of Card.t
  | Play of Card.t list (* Score.t *)
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
  let ( -- ) (xs : Card.t list) (ys : Card.t list) =
    Multiset.subtract xs ys ~equal:Card.equal
    |> Result.of_option ~error:`Not_enough_cards
  in 

  match (state, msg) with
  | Playing (hands, level, finished, turn, current), Sort ->
      let rank = rank_of level in
      Ok
        ( Playing
            ( hands
              |> Player.set player
                   (Player.get player hands
                   |> List.sort ~compare:(Card.compare_at rank)),
              level,
              finished,
              turn,
              current ),
          [] )
  | Trading (hands, level, position, remaining), Sort ->
      let rank = rank_of level in
      Ok
        ( Trading
            ( hands
              |> Player.set player
                   (Player.get player hands
                   |> List.sort ~compare:(Card.compare_at rank)),
              level,
              position,
              remaining ),
          [] )
  | Trading (_, _, _, remaining), Trade _ when Player.get player remaining = 0
    ->
      Error `Already_traded
  | Trading _, Revolt -> Error `Not_implemented (* TODO: implement *)
  | Trading (hands, level, position, remaining), Trade card -> (
      let hand = Player.get player hands in
      let* () =
        let level = rank_of level in
        (* Ensure the correct quantity of cards are being traded,
           and ensure that slaves give their highest cards *)
        match (Player.get player position, Rankj.of_card card) with
        | Big_master, _ | Small_master, _ -> Ok ()
        | Small_slave, offered | Big_slave, offered ->
            if
              hand
              |> List.filter ~f:(Fn.non @@ Card.equal (R (level, Hearts)))
              |> List.map ~f:Rankj.of_card
              |> List.max_elt ~compare:(Rankj.compare_at level)
              |> Option.value_map ~default:true ~f:(fun high ->
                     Rankj.compare_at level high offered <= 0)
            then Ok ()
            else Error `Must_give_highest_cards
      in

      let receiver : Player.t =
        match Player.get player position with
        | Big_master -> Player.who_is Big_slave position
        | Big_slave -> Player.who_is Big_master position
        | Small_master -> Player.who_is Small_slave position
        | Small_slave -> Player.who_is Small_master position
      in
      let* hand' = hand -- [ card ] in
      let hands' =
        hands |> Player.set player hand'
        |> Player.set receiver (card :: Player.get receiver hands)
      in
      match
        Player.set player (Int.( - ) (Player.get player remaining) 1) remaining
      with
      | Store (0, 0, 0, 0) ->
          Ok
            ( Playing
                (hands', level, No_one, Player.who_is Big_slave position, None),
              [ `Trade (player, receiver, card) ] )
      | traded' ->
          Ok
            ( Trading (hands', level, position, traded'),
              [ `Trade (player, receiver, card) ] ))
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
            let turn''' =
              if Player.get turn'' hands |> List.is_empty then Player.next turn'
              else turn''
            in
            Playing (hands, level, finished, turn''', None)
        | true, false -> next_state turn'
        | false, true -> Playing (hands, level, finished, turn', None)
        | false, false -> Playing (hands, level, finished, turn', current)
      in

      Ok (next_state turn, [ `Passed player ])
  | Playing (hands, level, finished, turn, current), Play (* score', *) cards'
    -> (
      let* score' =
        match Score.infer (rank_of level) (* score', *) cards' with
        | [] -> Error `Wrong_score
        | sc :: _ -> Ok sc
      in
      let* () =
        match current with
        | Some (_, score) when not (Score.lt_at (rank_of level) score score') ->
            Error `Doesn't_beat
        | _ -> Ok ()
      in
      let* hand' = Player.get player hands -- cards' in
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
          let remaining' = Player.(init 2 |> set sm 1 |> set player 1) in
          Ok
            ( Trading (deal_hands (), level', position', remaining'),
              [ `Played (player, score'); `New_round position' ] ))
  | Winner _, _
  | Trading _, Play _
  | Trading _, Pass
  | Playing _, Revolt
  | Playing _, Trade _ ->
      Error `Wrong_phase

let to_string : t -> string =
  let pts : Player.position -> string = function
    | Big_master -> "  Big master"
    | Small_master -> "Small master"
    | Small_slave -> "Small slave "
    | Big_slave -> "  Big slave "
  in
  let cts (cards : Card.t list) =
    String.concat ~sep:" " (List.map cards ~f:Card.to_string)
  in
  let lts : level -> string = function
    | Level (r, _, AC) -> Printf.sprintf "%s (AC)" (Rank.to_string r)
    | Level (_, r, BD) -> Printf.sprintf "%s (BD)" (Rank.to_string r)
  in
  let curts : (Player.t * Score.t) option -> string = function
    | None -> "leads"
    | Some (owner, score) ->
        Printf.sprintf "to beat %s's %s" (Player.show owner) (Score.show score)
  in
  function
  | Winner level -> Printf.sprintf "Winner: %s" (show_level level)
  | Playing (Store (a, b, c, d), level, _, turn, current) ->
      Printf.sprintf
        "[Playing] Level: %s\nA | %s\nB |  %s\nC |  %s\nD |  %s\n%s %s"
        (lts level) (cts a) (cts b) (cts c) (cts d) (Player.show turn)
        (curts current)
  | Trading (Store (a, b, c, d), level, position, remaining) ->
      Printf.sprintf
        "[Trading] Level: %s\n\
         A | %s | %d |  %s\n\
         B | %s | %d |  %s\n\
         C | %s | %d |  %s\n\
         D | %s | %d |  %s\n"
        (lts level)
        (Player.get A position |> pts)
        (Player.get A remaining) (cts a)
        (Player.get B position |> pts)
        (Player.get B remaining) (cts b)
        (Player.get C position |> pts)
        (Player.get C remaining) (cts c)
        (Player.get D position |> pts)
        (Player.get D remaining) (cts d)
