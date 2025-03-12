open Base
open Guandan
open Stdio

let game = ref (Game.new_game ())

let rec main () =
  let ( let* ) x f = Result.bind x ~f in
  let parse_card (str : string) : Types.Card.t option =
    match String.to_list str with
    | [ 'b'; 'j' ] -> Some (J Red)
    | [ 's'; 'j' ] -> Some (J Black)
    | [ r; s ] ->
        let rank : Types.Rank.t option =
          match r with
          | '2' -> Some Two
          | '3' -> Some Three
          | '4' -> Some Four
          | '5' -> Some Five
          | '6' -> Some Six
          | '7' -> Some Seven
          | '8' -> Some Eight
          | '9' -> Some Nine
          | 't' | 'T' -> Some Ten
          | 'j' | 'J' -> Some Jack
          | 'q' | 'Q' -> Some Queen
          | 'k' | 'K' -> Some King
          | 'a' | 'A' -> Some Ace
          | _ -> None
        in
        let suit : Types.Suit.t option =
          match s with
          | 'c' | 'C' -> Some Clubs
          | 'd' | 'D' -> Some Diamonds
          | 's' | 'S' -> Some Spades
          | 'h' | 'H' -> Some Hearts
          | _ -> None
        in
        Option.map2 rank suit ~f:(fun r s -> Types.Card.R (r, s))
    | _ -> None
  in
  let rec parse_cards = function
    | [] -> Some []
    | cs :: rest -> Option.map2 (parse_card cs) (parse_cards rest) ~f:List.cons
  in
  let parse_message (msg : string) =
    let string_split = String.split msg ~on:' ' in
    let* (player, rest) : Player.t * string list =
      match string_split with
      | "a" :: rest -> Ok (Player.A, rest)
      | "b" :: rest -> Ok (Player.B, rest)
      | "c" :: rest -> Ok (Player.C, rest)
      | "d" :: rest -> Ok (Player.D, rest)
      | _ -> Error `Invalid_player
    in
    let* msg : Game.message =
      match rest with
      | [ "sort" ] -> Ok Game.Sort
      | [ "pass" ] -> Ok Game.Pass
      | "play" :: cards -> (
          match parse_cards cards with
          | None -> Error `Couldn't_parse_cards
          | Some cs -> Ok (Game.Play cs))
      | [ "trade"; card ] -> (
          match parse_card card with
          | None -> Error `Couldn't_parse_cards
          | Some cs -> Ok (Game.Trade cs))
      | [ "revolt" ] -> Error `Not_implemented
      | _ -> Error `Invalid_command
    in
    Ok (player, msg)
  in
  print_endline (Game.to_string !game);
  print_string ">>> ";
  Out_channel.flush Out_channel.stdout;
  match In_channel.input_line In_channel.stdin with
  | Some "q" | Some "quit" -> print_endline "Bye"
  | Some input ->
      (match parse_message input with
      | Ok (player, message) -> (
          match Game.transition !game player message with
          | Ok (game', _) -> game := game'
          | Error `Already_traded -> Stdio.prerr_endline "Already traded"
          | Error `Cannot_pass -> Stdio.prerr_endline "Cannot pass"
          | Error `Doesn't_beat -> Stdio.prerr_endline "Doesn't beat"
          | Error `Incorrent_number_of_cards ->
              Stdio.prerr_endline "Incorrect number of cards"
          | Error `Must_give_highest_cards ->
              Stdio.prerr_endline "Must give highest cards"
          | Error `Not_enough_cards -> Stdio.prerr_endline "Not enough cards"
          | Error `Not_implemented -> Stdio.prerr_endline "Not implemented"
          | Error `Not_your_turn -> Stdio.prerr_endline "Not your turn"
          | Error `Wrong_phase -> Stdio.prerr_endline "Wrong phase"
          | Error `Wrong_score -> Stdio.prerr_endline "Wrong score"
          | Error _ -> Stdio.prerr_endline "Internal Error")
      | Error `Invalid_command -> Stdio.prerr_endline "Invalid command"
      | Error `Invalid_player -> Stdio.prerr_endline "Invalid player"
      | Error `Couldn't_parse_cards ->
          Stdio.prerr_endline "Couldn't parse cards"
      | Error `Not_implemented -> Stdio.prerr_endline "Not implemented");
      main ()
  | None -> ()

let () = main ()
