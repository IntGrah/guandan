open Base
open Guandan.Multiset

let ( = ) = Option.equal (List.equal equal)
let ( - ) = subtract ~equal
let () = assert ([] - [ 1 ] = None)
let () = assert ([ 2 ] - [] = Some [ 2 ])
let () = assert ([ 3 ] - [ 3 ] = Some [])
let () = assert ([ 4; 4 ] - [ 4 ] = Some [ 4 ])

let () =
  assert (
    [ 5; 4; 6; 1; 8; 8; 9; 1; 6 ] - [ 1; 6; 8; 1; 8 ] = Some [ 5; 4; 9; 6 ])
