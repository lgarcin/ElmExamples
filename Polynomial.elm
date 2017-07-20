module Polynomial exposing (..)

import Html exposing (Html, text)
import List exposing (..)


type alias Polynomial =
    List Float


type Degree
    = MinusInfinity
    | Deg Int


type Valuation
    = PlusInfinity
    | Val Int


eval : Polynomial -> Float -> Float
eval p x =
    case p of
        [] ->
            0

        e :: q ->
            e + x * (eval q x)


add : Polynomial -> Polynomial -> Polynomial
add p q =
    case ( p, q ) of
        ( [], q1 ) ->
            q1

        ( p1, [] ) ->
            p1

        ( e :: p1, f :: q1 ) ->
            let
                s =
                    add p1 q1
            in
                case s of
                    [] ->
                        if e + f == 0 then
                            []
                        else
                            [ e + f ]

                    _ :: _ ->
                        (e + f) :: s


sub : Polynomial -> Polynomial -> Polynomial
sub p q =
    add p (mul1 -1 q)


mul1 : Float -> Polynomial -> Polynomial
mul1 c p =
    if c == 0 then
        []
    else
        map (\x -> c * x) p


mul2 : Polynomial -> Polynomial
mul2 p =
    case p of
        [] ->
            []

        _ :: _ ->
            0 :: p


mul : Polynomial -> Polynomial -> Polynomial
mul p q =
    case ( p, q ) of
        ( [], _ ) ->
            []

        ( e :: p1, q1 ) ->
            add (mul1 e q1) (mul2 (mul p1 q1))


degree : Polynomial -> Degree
degree p =
    case p of
        [] ->
            MinusInfinity

        e :: q ->
            case degree q of
                MinusInfinity ->
                    if e == 0 then
                        MinusInfinity
                    else
                        Deg 0

                Deg d ->
                    Deg (1 + d)


valuation : Polynomial -> Valuation
valuation p =
    case p of
        [] ->
            PlusInfinity

        e :: q ->
            if e == 0 then
                case valuation q of
                    PlusInfinity ->
                        PlusInfinity

                    Val v ->
                        Val (1 + v)
            else
                Val 0


deriv : Polynomial -> Polynomial
deriv p =
    case p of
        [] ->
            []

        e :: q ->
            add (mul2 (deriv q)) q


integr1 : Polynomial -> Int -> Polynomial
integr1 p n =
    case p of
        [] ->
            []

        e :: q ->
            e / toFloat (n + 1) :: integr1 q (n + 1)


integr : Polynomial -> Polynomial
integr p =
    mul2 (integr1 p 0)


main : Html a
main =
    integr [ 1, 1, 1 ] |> toString |> text
