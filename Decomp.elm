module Decomp exposing (..)


import Html exposing (Html,text)

decomp: Int -> List Int
decomp n =
  case n of
    0 -> []
    _ -> (n%2)::decomp(n//2)

pgcd: Int -> Int -> Int
pgcd a b =
  case b of
    0 -> a
    _ -> pgcd b (a%b)

euclide: Int -> Int -> (Int, Int, Int)
euclide a b =
    case b of
    0 -> 1
    _ -> 2


main : Html a
main =
  (decomp 1234, pgcd 15 27) |> toString |> text
