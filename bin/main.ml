open Base

let () = Card.tests ()
let () = Joker.tests ()
let () = Multiset.tests ()
let () = Player.tests ()
let () = Rank.tests ()
let () = Rankj.tests ()
let () = Score.tests ()
let () = Suit.tests ()
let () = Stdio.print_endline @@ Game.show (Game.new_game ())
