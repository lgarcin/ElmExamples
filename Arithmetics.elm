module Arithmetics exposing (..)

import Html exposing (Html, text)


decomp : Int -> Int -> List Int
decomp n b =
    case n of
        0 ->
            []

        _ ->
            (n % b) :: decomp (n // b) b


pgcd : Int -> Int -> Int
pgcd a b =
    case b of
        0 ->
            abs (a)

        _ ->
            pgcd b (a % b)


div : Int -> Int -> ( Int, Int )
div a b =
    let
        r =
            a % b
    in
        ( (a - r) // b, r )


euclide : Int -> Int -> ( Int, Int, Int )
euclide a b =
    case b of
        0 ->
            if a < 0 then
                ( -a, -1, 0 )
            else
                ( a, 1, 0 )

        _ ->
            let
                ( q, r ) =
                    div a b

                ( d, u, v ) =
                    euclide b r
            in
                ( d, v, u - q * v )


main : Html a
main =
    ( decomp 1234 10, euclide 458 473 ) |> toString |> text
