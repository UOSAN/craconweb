module Result.DotProbeTests exposing (all)

import Expect
import Game.Result
import Test exposing (..)


all : Test
all =
    describe "DotProbe Tests"
        [ test "Incorrect" <|
            \() ->
                Expect.false "Incorrect"
                    (Game.Result.isCorrect { gameSlug = "dotprobe" }
                        { id = Nothing
                        , sessionId = "sessionId"
                        , sort = 0
                        , fixation = Just 1495251474320
                        , selection = Just 1495251476011
                        , pictures = Just 1495251474839
                        , redcross = Nothing
                        , probe = Just 1495251475338
                        , border = Nothing
                        , timeout = Nothing
                        , rest = Just 1495251476011
                        , interval = Nothing
                        , width = Just 2
                        , height = Nothing
                        , blue = False
                        , gray = False
                        , dash = False
                        , targetIndex = 0
                        , selectedIndex = 1
                        , startIndex = 0
                        , images = [ "7169127603820459510", "7245635463824259514" ]
                        }
                    )
        , test "Correct" <|
            \() ->
                Expect.true "Correct"
                    (Game.Result.isCorrect { gameSlug = "dotprobe" }
                        { id = Nothing
                        , sessionId = "sessionId"
                        , sort = 0
                        , fixation = Just 1495252338828
                        , selection = Just 1495252340664
                        , pictures = Just 1495252339330
                        , redcross = Nothing
                        , probe = Just 1495252339828
                        , border = Nothing
                        , timeout = Nothing
                        , rest = Just 1495252340664
                        , interval = Nothing
                        , width = Just 2
                        , height = Nothing
                        , blue = False
                        , gray = False
                        , dash = False
                        , targetIndex = 0
                        , selectedIndex = 0
                        , startIndex = 0
                        , images = [ "7679994453820479507", "7754139733824279513" ]
                        }
                    )
        ]
