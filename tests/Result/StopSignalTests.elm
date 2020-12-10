module Result.StopSignalTests exposing (all)

import Expect
import Game exposing (BorderType(..), Direction(..), Layout(..), LogEntry(..))
import Game.Result
import Test exposing (..)


all : Test
all =
    describe "StopSignal Tests"
        [ test "Correct Response" <|
            \() ->
                Expect.true "Correct Response"
                    (Game.Result.isCorrect { gameSlug = "stopsignal" }
                        { id = Nothing
                        , sessionId = "sessionId"
                        , sort = 0
                        , fixation = Nothing
                        , selection = Just 1495252956461
                        , pictures = Just 1495252955885
                        , redcross = Nothing
                        , probe = Nothing
                        , border = Just 1495252955986
                        , timeout = Nothing
                        , rest = Just 1495252956891
                        , interval = Nothing
                        , width = Nothing
                        , height = Nothing
                        , blue = True
                        , gray = False
                        , dash = False
                        , targetIndex = 0
                        , selectedIndex = 0
                        , startIndex = 0
                        , images = [ "3364256873820319513" ]
                        }
                    )
        , test "Correct Timeout" <|
            \() ->
                Expect.true "Correct Timeout"
                    (Game.Result.isCorrect { gameSlug = "stopsignal" }
                        { id = Nothing
                        , sessionId = "sessionId"
                        , sort = 0
                        , fixation = Nothing
                        , selection = Nothing
                        , pictures = Just 1495254189054
                        , redcross = Nothing
                        , probe = Nothing
                        , border = Just 1495254189159
                        , timeout = Just 1495254190064
                        , rest = Just 1495254190064
                        , interval = Nothing
                        , width = Nothing
                        , height = Nothing
                        , blue = False
                        , gray = True
                        , dash = False
                        , targetIndex = 0
                        , selectedIndex = 0
                        , startIndex = 0
                        , images = [ "1630240873824469514" ]
                        }
                    )
        , test "Incorrect Response" <|
            \() ->
                Expect.false "Incorrect Response"
                    (Game.Result.isCorrect { gameSlug = "stopsignal" }
                        { id = Nothing
                        , sessionId = "sessionId"
                        , sort = 0
                        , fixation = Nothing
                        , selection = Just 1495254367455
                        , pictures = Just 1495254366705
                        , redcross = Just 1495254367706
                        , probe = Nothing
                        , border = Just 1495254366809
                        , timeout = Nothing
                        , rest = Just 1495254368205
                        , interval = Nothing
                        , width = Nothing
                        , height = Nothing
                        , blue = False
                        , gray = True
                        , dash = False
                        , targetIndex = 0
                        , selectedIndex = 0
                        , startIndex = 0
                        , images = [ "1479458643823499512" ]
                        }
                    )
        , test "Incorrect Timeout" <|
            \() ->
                Expect.false "Incorrect Timeout"
                    (Game.Result.isCorrect { gameSlug = "stopsignal" }
                        { id = Nothing
                        , sessionId = "sessionId"
                        , sort = 0
                        , fixation = Nothing
                        , selection = Nothing
                        , pictures = Just 1495254548968
                        , redcross = Just 1495254549976
                        , probe = Nothing
                        , border = Just 1495254549077
                        , timeout = Just 1495254549976
                        , rest = Just 1495254550472
                        , interval = Nothing
                        , width = Nothing
                        , height = Nothing
                        , blue = True
                        , gray = False
                        , dash = False
                        , targetIndex = 0
                        , selectedIndex = 0
                        , startIndex = 0
                        , images = [ "7169127603820459510" ]
                        }
                    )
        ]
