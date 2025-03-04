type t = Diamonds | Clubs | Hearts | Spades [@@deriving show, eq, ord]

let all : t list = [ Clubs; Hearts; Diamonds; Spades ]

let tests () =
  assert (compare Diamonds Clubs < 0);
  assert (compare Clubs Hearts < 0);
  assert (compare Hearts Spades < 0);
  assert (compare Spades Hearts > 0);
  assert (compare Hearts Clubs > 0);
  assert (compare Clubs Diamonds > 0);
  assert (compare Spades Spades = 0);
  Stdio.print_endline "Suit: tests passed"
