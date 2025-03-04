open Base

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

let tests () =
  let ( = ) = Option.equal (List.equal equal) in
  let ( - ) = subtract ~equal in
  assert ([] - [ 1 ] = None);
  assert ([ 2 ] - [] = Some [ 2 ]);
  assert ([ 3 ] - [ 3 ] = Some []);
  assert ([ 4; 4 ] - [ 4 ] = Some [ 4 ]);
  assert (
    [ 5; 4; 6; 1; 8; 8; 9; 1; 6 ] - [ 1; 6; 8; 1; 8 ] = Some [ 5; 4; 9; 6 ]);
  Stdio.print_endline "Multiset: tests passed"
