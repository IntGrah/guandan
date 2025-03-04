open Base

let () = Card.tests ()
let () = Game.tests ()
let () = Joker.tests ()
let () = Player.tests ()
let () = Rank.tests ()
let () = Rankj.tests ()
let () = Score.tests ()
let () = Suit.tests ()

(* let () = Stdio.print_endline @@ Game.show (Game.new_game ()) *)
let g () : Game.t =
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
          Trading
            {
              position =
                {
                  p1 = Game.Position.Big_slave;
                  p2 = Game.Position.Small_slave;
                  p3 = Game.Position.Small_master;
                  p4 = Game.Position.Big_master;
                };
              traded = { p1 = false; p2 = false; p3 = false; p4 = false };
            } )
  | _ -> failwith "Should have 108 cards"

let game = g ()
let () = Stdio.print_endline @@ Game.show game
