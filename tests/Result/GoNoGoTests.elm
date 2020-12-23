module Result.GoNoGoTests exposing (all)

import Expect
import Game.Result
import Test exposing (..)


all : Test
all =
    describe "GoNoGo Tests"
        [ goTests

        -- , noGoTests
        ]


goTests : Test
goTests =
    describe "Go"
        [ test "Timeout Go" <|
            \() ->
                Expect.false "Timeout is not a correct answer"
                    (Game.Result.isCorrect { gameSlug = "gonogo" }
                        { id = Nothing
                        , sessionId = "sessionId"
                        , sort = 0
                        , fixation = Nothing
                        , selection = Nothing
                        , pictures = Just 1495162244273
                        , redcross = Just 1495162245538
                        , probe = Nothing
                        , border = Just 1495162244273
                        , timeout = Just 1495162245538
                        , rest = Just 1495162246038
                        , interval = Nothing
                        , width = Just 2
                        , height = Nothing
                        , blue = False
                        , gray = False
                        , dash = False
                        , targetIndex = 1
                        , selectedIndex = 0
                        , startIndex = 1
                        , images = [ "6920533513820449510" ]
                        }
                    )
        , test "Correct Go" <|
            \() ->
                Expect.true "Correct Go"
                    (Game.Result.isCorrect { gameSlug = "gonogo" }
                        { id = Nothing
                        , sessionId = "sessionId"
                        , sort = 0
                        , fixation = Nothing
                        , selection = Just 1495240100609
                        , pictures = Just 1495240100139
                        , redcross = Nothing
                        , probe = Nothing
                        , border = Just 1495240100139
                        , timeout = Nothing
                        , rest = Just 1495240100609
                        , interval = Nothing
                        , width = Just 2
                        , height = Nothing
                        , blue = False
                        , gray = False
                        , dash = False
                        , targetIndex = 0
                        , selectedIndex = 0
                        , startIndex = 0
                        , images = [ "2543813863820289509" ]
                        }
                    )
        , test "Incorrect Go" <|
            \() ->
                Expect.false "Incorrect Go"
                    (Game.Result.isCorrect { gameSlug = "gonogo" }
                        { id = Nothing
                        , sessionId = "sessionId"
                        , sort = 0
                        , fixation = Nothing
                        , selection = Just 1495248757557
                        , pictures = Just 1495248756701
                        , redcross = Just 1495248757557
                        , probe = Nothing
                        , border = Just 1495248756701
                        , timeout = Nothing
                        , rest = Just 1495248758455
                        , interval = Nothing
                        , width = Just 2
                        , height = Nothing
                        , blue = False
                        , gray = False
                        , dash = False
                        , targetIndex = 1
                        , selectedIndex = 0
                        , startIndex = 1
                        , images = [ "755506963820219512" ]
                        }
                    )
        ]


noGoTests : Test
noGoTests =
    describe "NoGo"
        [ test "Timeout/Correct NoGo" <|
            \() ->
                Expect.true "Timeout nogo is correct"
                    (Game.Result.isCorrect { gameSlug = "gonogo" }
                        { id = Nothing
                        , sessionId = "sessionId"
                        , sort = 0
                        , fixation = Nothing
                        , selection = Nothing
                        , pictures = Just 1495249097925
                        , redcross = Nothing
                        , probe = Nothing
                        , border = Just 1495249097925
                        , timeout = Just 1495249099189
                        , rest = Just 1495249099189
                        , interval = Nothing
                        , width = Just 2
                        , height = Nothing
                        , blue = False
                        , gray = False
                        , dash = True
                        , targetIndex = 1
                        , selectedIndex = 0
                        , startIndex = 1
                        , images = [ "4615715483824159516" ]
                        }
                    )
        , test "Incorrect NoGo" <|
            \() ->
                Expect.false "Incorrect NoGo"
                    (Game.Result.isCorrect { gameSlug = "gonogo" }
                        { id = Nothing
                        , sessionId = "sessionId"
                        , sort = 0
                        , fixation = Nothing
                        , selection = Just 1495250615497
                        , pictures = Just 1495250615013
                        , redcross = Just 1495250615497
                        , probe = Nothing
                        , border = Just 1495250615013
                        , timeout = Nothing
                        , rest = Just 1495250616779
                        , interval = Nothing
                        , width = Just 2
                        , height = Nothing
                        , blue = False
                        , gray = False
                        , dash = True
                        , targetIndex = 1
                        , selectedIndex = 1
                        , startIndex = 1
                        , images = [ "4260027113823189510" ]
                        }
                    )
        ]
