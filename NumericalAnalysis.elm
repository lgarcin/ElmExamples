module NumericalAnalysis exposing (..)

import Html exposing (Html, text)


root : (Float -> Float) -> Float -> Float -> Float -> Maybe Float
root f a b eps =
    if (f a) * (f b) > 0 then
        Nothing
    else
        let
            c =
                (a + b) / 2
        in
            if abs (b - a) <= eps then
                Just c
            else if (f c) * (f a) < 0 then
                root f a c eps
            else
                root f c b eps


main : Html a
main =
    root cos 0 3 0.00001 |> toString |> text
