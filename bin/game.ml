open Base

module Player = struct
  type t = P1 | P2 | P3 | P4 [@@deriving show, eq]

  let next : t -> t = function P1 -> P2 | P2 -> P3 | P3 -> P4 | P4 -> P1
  let teammate (p : t) : t = next (next p)
end

module Store = struct
  type 'a t = { p1 : 'a; p2 : 'a; p3 : 'a; p4 : 'a } [@@deriving show]

  let get (p : Player.t) (store : 'a t) : 'a =
    match p with
    | P1 -> store.p1
    | P2 -> store.p2
    | P3 -> store.p3
    | P4 -> store.p4

  let set (p : Player.t) (v : 'a) (store : 'a t) : 'a t =
    match p with
    | P1 -> { store with p1 = v }
    | P2 -> { store with p2 = v }
    | P3 -> { store with p3 = v }
    | P4 -> { store with p4 = v }

  let find (v : 'a) { p1 = v1; p2 = v2; p3 = v3; p4 = v4 } ~equal : Player.t =
    if equal v v1 then P1
    else if equal v v2 then P2
    else if equal v v3 then P3
    else if equal v v4 then P4
    else failwith "Not found"

  let for_all { p1 = v1; p2 = v2; p3 = v3; p4 = v4 } ~f =
    f v1 && f v2 && f v3 && f v4
end

module Position = struct
  type t = Big_master | Small_master | Small_slave | Big_slave
  [@@deriving show, eq]

  let who_is (pos : t) (store : t Store.t) = Store.find pos store ~equal
end

type t = Active of Card.t list Store.t * levels * phase | Done
[@@deriving show]

and levels = { level : Rank.t; level_13 : Rank.t; level_24 : Rank.t }
[@@deriving show]

and phase =
  | Trading of Position.t Store.t * bool Store.t
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

type action = None | Broadcast of string | PrivateMessage of Player.t * string
[@@deriving show]

let new_game () : t =
  let deck =
    Card.all @ Card.all
    |> List.map ~f:(fun c -> (Random.bits (), c))
    |> List.sort ~compare:(fun (b0, _) (b1, _) -> Int.compare b0 b1)
    |> List.map ~f:snd
  in

  match List.chunks_of ~length:27 deck with
  | [ h1; h2; h3; h4 ] ->
      Active
        ( { p1 = h1; p2 = h2; p3 = h3; p4 = h4 },
          { level = Two; level_13 = Two; level_24 = Two },
          Playing { turn = P1; current = Lead } )
  | _ -> failwith "Should have 108 cards"

let rec subtract (xs : 'a list) (ys : 'a list) ~equal : 'a list option =
  let rec sub (xs : 'a list) (y : 'a) : 'a list option =
    match xs with
    | [] -> None
    | h :: hs when equal h y -> Some hs
    | h :: hs -> sub hs y |> Option.map ~f:(List.cons h)
  in
  match (xs, ys) with
  | xs, [] -> Some xs
  | [], _ -> None
  | xs, y :: ys ->
      sub xs y |> Option.value ~default:xs |> Fn.flip subtract ys ~equal

let transition (player : Player.t) (state : t) (msg : message) :
    (t * action, _) Result.t =
  let ( - ) = subtract ~equal:Card.equal in
  match state with
  | Done -> Error `Wrong_phase
  | Active (hands, levels, phase) -> (
      match phase with
      | Trading (pos, traded) -> (
          let phase =
            let traded = Store.set player true traded in
            if Store.for_all traded ~f:Fn.id then
              Playing { turn = Position.who_is Big_slave pos; current = Lead }
            else Trading (pos, traded)
          in

          match (msg, Store.get player pos) with
          | Play _, _ | Pass, _ -> Error `Wrong_phase
          | (Trade1 _, _ | Trade2 _, _) when Store.get player traded ->
              Error `Already_traded
          | Trade1 _, Big_master
          | Trade1 _, Big_slave
          | Trade2 _, Small_master
          | Trade2 _, Small_slave
          | Revolt, Big_master
          | Revolt, Small_master ->
              Error `Wrong_position
          | Trade1 c, Small_master -> (
              let hand = Store.get player hands in
              let small_slave = Position.who_is Small_slave pos in
              let hand_small_slave = Store.get small_slave hands in
              match hand - [ c ] with
              | None -> Error `Not_enough_cards
              | Some hand' ->
                  Ok
                    ( Active
                        ( hands |> Store.set player hand'
                          |> Store.set small_slave (c :: hand_small_slave),
                          levels,
                          phase ),
                      Broadcast "Small master gave to small slave" ))
          | Trade1 c, Small_slave -> (
              let hand = Store.get player hands in
              let small_master = Position.who_is Small_master pos in
              let hand_small_master = Store.get small_master hands in
              match hand - [ c ] with
              | None -> Error `Not_enough_cards
              | Some hand' ->
                  Ok
                    ( Active
                        ( hands |> Store.set player hand'
                          |> Store.set small_master (c :: hand_small_master),
                          levels,
                          phase ),
                      Broadcast "Small slave gave to small master" ))
          | Trade2 (c0, c1), Big_master -> (
              let hand = Store.get player hands in
              let big_slave = Position.who_is Big_slave pos in
              let hand_big_slave = Store.get big_slave hands in
              match hand - [ c0; c1 ] with
              | None -> Error `Not_enough_cards
              | Some hand' ->
                  Ok
                    ( Active
                        ( hands |> Store.set player hand'
                          |> Store.set big_slave (c0 :: c1 :: hand_big_slave),
                          levels,
                          phase ),
                      Broadcast "Small master gave to small slave" ))
          | Trade2 (c0, c1), Big_slave -> (
              let hand = Store.get player hands in
              let big_master = Position.who_is Big_master pos in
              let hand_big_master = Store.get big_master hands in
              match hand - [ c0; c1 ] with
              | None -> Error `Not_enough_cards
              | Some hand' ->
                  Ok
                    ( Active
                        ( hands |> Store.set player hand'
                          |> Store.set big_master (c0 :: c1 :: hand_big_master),
                          levels,
                          phase ),
                      Broadcast "Small slave gave to small master" ))
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
              Ok (Active (hands, levels, phase), Broadcast "Player passed")
          | Play (sc, cs), _ when not (Score.verify sc cs) -> Error `Wrong_score
          | Play (sc, _), Resp (_, sc')
            when not (Score.lt_at levels.level sc' sc) ->
              Error `Doesn't_beat
          | Play (sc, cs), Resp (_, _) | Play (sc, cs), Lead -> (
              let hand = Store.get player hands in
              match hand - cs with
              | None -> Error `Not_enough_cards
              | Some hand ->
                  (* TODO: detect when a playing phase is over *)
                  let phase =
                    Playing
                      { turn = Player.next turn; current = Resp (player, sc) }
                  in
                  Ok
                    ( Active (Store.set player hand hands, levels, phase),
                      Broadcast "Player played" ))))

let tests () =
  let ( = ) = Option.equal (List.equal equal) in
  let ( - ) = subtract ~equal in
  assert ([] - [ 1 ] = None);
  assert ([ 2 ] - [] = Some [ 2 ]);
  assert ([ 3 ] - [ 3 ] = Some []);
  assert ([ 4; 4 ] - [ 4 ] = Some [ 4 ]);
  assert (
    [ 5; 4; 6; 1; 8; 8; 9; 1; 6 ] - [ 1; 6; 8; 1; 8 ] = Some [ 5; 4; 9; 6 ]);
  Stdio.print_endline "Game: tests passed"
