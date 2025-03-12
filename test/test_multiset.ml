open Base
open Guandan.Multiset

let ( = ) = Option.equal (List.equal equal)
let ( - ) = subtract ~equal

let () =
  assert ([] - [ 1 ] = None);
  assert ([ 2 ] - [] = Some [ 2 ]);
  assert ([ 3 ] - [ 3 ] = Some []);
  assert ([ 4; 4 ] - [ 4 ] = Some [ 4 ]);
  assert (
    [ 5; 4; 6; 1; 8; 8; 9; 1; 6 ] - [ 1; 6; 8; 1; 8 ] = Some [ 5; 4; 9; 6 ]);
  assert ([ 5; 4; 6; 1; 8; 8; 9; 1; 6 ] - [ 1; 6; 8; 1; 8; 1 ] = None)
