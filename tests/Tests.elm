module Tests exposing (..)

import Expect exposing (..)
import MathParser exposing (..)
import Test exposing (..)


suite : Test
suite =
    describe "MathParser Tests"
        [ test "1 + 2" <|
            \() ->
                parse "1 + 2"
                    |> Expect.equal (Ok (Add (Integer 1) (Integer 2)))
        , test "1 - 2" <|
            \() ->
                parse "1 - 2"
                    |> Expect.equal (Ok (Sub (Integer 1) (Integer 2)))
        , test "1 - 2 * 5" <|
            \() ->
                parse "1 - 2 x 5"
                    |> Expect.equal (Ok (Sub (Integer 1) (Mul (Integer 2) (Integer 5))))
        , test "1 - 10 / 5" <|
            \() ->
                parse "1 - 2 / 5"
                    |> Expect.equal (Ok (Sub (Integer 1) (Div (Integer 2) (Integer 5))))
        , test "(2 - 10) / 5" <|
            \() ->
                parse "(2 - 10) / 5"
                    |> Expect.equal (Ok (Div (Sub (Integer 2) (Integer 10)) (Integer 5)))
        , test "(4 - 10) / 3" <|
            \() ->
                parse "(4 - 10) / 3"
                    |> Result.withDefault (Integer 0)
                    |> evaluate
                    |> Expect.equal -2
        , test "-(3 - 2)" <|
            \() ->
                parse "-(3 - 2)"
                    |> Result.withDefault (Integer 0)
                    |> evaluate
                    |> Expect.equal -1
        , test "-3 - 2" <|
            \() ->
                parse "-3 - 2"
                    |> Result.withDefault (Integer 0)
                    |> evaluate
                    |> Expect.equal -5
        , test "sqrt(4)" <|
            \() ->
                parse "sqrt(4)"
                    |> Result.withDefault (Integer 0)
                    |> evaluate
                    |> Expect.equal 2
        , test "3 - sqrt(4)" <|
            \() ->
                parse "3 - sqrt(4)"
                    |> Result.withDefault (Integer 0)
                    |> evaluate
                    |> Expect.equal 1
        , test "eval: 3 - sqrt(4 ^ 2) x 2 ^ 3 + 32" <|
            \() ->
                parse "3 - sqrt(4 ^ 2) x 2 ^ 3 + 32"
                    |> Result.withDefault (Integer 1)
                    |> evaluate
                    |> Expect.equal 3
        ]
