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
  | xs, y :: ys -> sub xs y |> Option.bind ~f:(Fn.flip subtract ys ~equal)
