open Base
open Guandan

let () = Stdio.print_endline @@ Game.show (Game.new_game ())
let _ = List.iter

(* let string_of_client client =
  client |> Ws.Client.id |> Ws.Client.Id.to_int |> Int.to_string *)

(* let server = Ws.Server.create ~port:3000 *)

(* let on_connect client =
  Ws.Client.send client {|Commands:
/msg [user] [message]
/list
/quit|}

let handler client message =
  match String.split_on_chars ~on:[ ' ' ] message with
  | [ "/list" ] ->
      let online =
        Ws.Server.clients server
        |> List.map ~f:string_of_client
        |> String.concat ~sep:"\n"
      in
      Ws.Client.send client ("Online:\n" ^ online)
  | [ "/quit" ] -> Ws.Server.close server client
  | "/msg" :: user :: content -> (
      let target =
        match Int.of_string_opt user with
        | Some n -> Ws.(Server.get server (Client.Id.of_int n))
        | None -> None
      in
      match target with
      | None -> Ws.Client.send client "No user with that ID exists"
      | Some target ->
          Ws.Client.send target
            (Printf.sprintf "Message from %s: %s" (string_of_client client)
               (String.concat ~sep:" " content)))
  | broadcast_message ->
      Ws.Server.broadcast_to_others server client
        (Printf.sprintf "%s: %s" (string_of_client client)
           (String.concat ~sep:" " broadcast_message)) *)

(* let () = Lwt_main.run (Ws.Server.run ~on_connect server handler) *)
