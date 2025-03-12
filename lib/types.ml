open Base

module Joker = struct
  type t = Black | Red [@@deriving show, eq, ord]
end

module Rank = struct
  type t =
    | Two
    | Three
    | Four
    | Five
    | Six
    | Seven
    | Eight
    | Nine
    | Ten
    | Jack
    | Queen
    | King
    | Ace
  [@@deriving show, eq, ord]

  let to_string : t -> string = function
    | Two -> "2"
    | Three -> "3"
    | Four -> "4"
    | Five -> "5"
    | Six -> "6"
    | Seven -> "7"
    | Eight -> "8"
    | Nine -> "9"
    | Ten -> "T"
    | Jack -> "J"
    | Queen -> "Q"
    | King -> "K"
    | Ace -> "A"

  let all : t list =
    [
      Ace;
      Two;
      Three;
      Four;
      Five;
      Six;
      Seven;
      Eight;
      Nine;
      Ten;
      Jack;
      Queen;
      King;
    ]

  let consec (r0 : t) (r1 : t) : bool =
    match (r0, r1) with
    | Ace, Two
    | Two, Three
    | Three, Four
    | Four, Five
    | Five, Six
    | Six, Seven
    | Seven, Eight
    | Eight, Nine
    | Nine, Ten
    | Ten, Jack
    | Jack, Queen
    | Queen, King
    | King, Ace ->
        true
    | _ -> false

  let succ : t -> t = function
    | Two -> Three
    | Three -> Four
    | Four -> Five
    | Five -> Six
    | Six -> Seven
    | Seven -> Eight
    | Eight -> Nine
    | Nine -> Ten
    | Ten -> Jack
    | Jack -> Queen
    | Queen -> King
    | King | Ace -> Ace

  let compare_at (level : t) (r0 : t) (r1 : t) : int =
    match (equal r0 level, equal r1 level) with
    | true, true -> 0
    | true, false -> 1
    | false, true -> -1
    | false, false -> compare r0 r1
end

module Suit = struct
  type t = Diamonds | Clubs | Hearts | Spades [@@deriving show, eq, ord]

  let to_string : t -> string = function
    | Diamonds -> "♢"
    | Clubs -> "♧"
    | Hearts -> "♡"
    | Spades -> "♤"

  let all : t list = [ Clubs; Hearts; Diamonds; Spades ]
end

module Card = struct
  open Base

  type t = R of Rank.t * Suit.t | J of Joker.t [@@deriving show, eq, ord]

  let to_string : t -> string = function
    | R (r, s) -> Rank.to_string r ^ Suit.to_string s
    | J Red -> "BJ"
    | J Black -> "SJ"

  let all : t list =
    J Red :: J Black
    :: List.map (List.cartesian_product Rank.all Suit.all) ~f:(fun (r, s) ->
           R (r, s))

  let compare_at (level : Rank.t) (c0 : t) (c1 : t) : int =
    match (c0, c1) with
    | J j0, J j1 -> Joker.compare j0 j1
    | J _, R _ -> 1
    | R _, J _ -> -1
    | R (r0, s0), R (r1, s1) -> (
        match Rank.compare_at level r0 r1 with
        | 0 -> Suit.compare s0 s1
        | n -> n)
end

module Rankj = struct
  type t = R of Rank.t | J of Joker.t [@@deriving show, eq, ord]

  let of_card : Card.t -> t = function R (r, _) -> R r | J j -> J j

  let compare_at (level : Rank.t) (rj0 : t) (rj1 : t) : int =
    match (rj0, rj1) with
    | J j0, J j1 -> Joker.compare j0 j1
    | J _, R _ -> 1
    | R _, J _ -> -1
    | R r0, R r1 -> Rank.compare_at level r0 r1
end
