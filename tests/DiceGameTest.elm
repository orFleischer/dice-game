module DiceGameTest exposing (suite)

import DiceGame
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)


suite : Test
suite =
    describe "Testing renderHiScores"
        [
            test "empty hi scores will yield an empty html tree" <|
            \_ ->
                DiceGame.renderHiScores [] |> Expect.equal []
        ]
