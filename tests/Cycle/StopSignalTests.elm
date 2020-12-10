module Cycle.StopSignalTests exposing (all)

import Expect
import Game exposing (BorderType(..), Direction(..), Layout(..), LogEntry(..))
import Game.Cycle
import Test exposing (..)


all : Test
all =
    describe "StopSignal Tests"
        [ test "Correct Response" <|
            \() ->
                Expect.equal
                    [ { id = Nothing
                      , sessionId = "sessionId"
                      , sort = 0
                      , fixation = Nothing
                      , selection = Just 1495252956461
                      , pictures = Just 1495252955885
                      , redcross = Nothing
                      , probe = Nothing
                      , border = Just 1495252955986
                      , timeout = Nothing
                      , rest = Nothing
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
                    ]
                    (Game.Cycle.generate "sessionId"
                        [ EndTrial 1495252956891
                        , AcceptIndication { desired = True } 1495252956461
                        , BeginDisplay (Just (Single Blue { url = "http://localhost:8654/repo/219025ee1b1595137d40dc1998da83b7.png", id = "3364256873820319513" })) 1495252955986
                        , BeginInput 1495252955986
                        , BeginDisplay (Just (Single None { url = "http://localhost:8654/repo/219025ee1b1595137d40dc1998da83b7.png", id = "3364256873820319513" })) 1495252955885
                        , BeginTrial 1495252955885
                        ]
                    )
        , test "Correct Timeout" <|
            \() ->
                Expect.equal
                    [ { id = Nothing
                      , sessionId = "sessionId"
                      , sort = 0
                      , fixation = Nothing
                      , selection = Nothing
                      , pictures = Just 1495254189054
                      , redcross = Nothing
                      , probe = Nothing
                      , border = Just 1495254189159
                      , timeout = Just 1495254190064
                      , rest = Nothing
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
                    ]
                    (Game.Cycle.generate "sessionId"
                        [ EndTrial 1495254190064
                        , Timeout { desired = True } 1495254190064
                        , BeginDisplay (Just (Single Gray { url = "http://localhost:8654/repo/1f5ce3a60432bc27e6984aaf624ea985.png", id = "1630240873824469514" })) 1495254189159
                        , BeginInput 1495254189159
                        , BeginDisplay (Just (Single None { url = "http://localhost:8654/repo/1f5ce3a60432bc27e6984aaf624ea985.png", id = "1630240873824469514" })) 1495254189054
                        , BeginTrial 1495254189054
                        ]
                    )
        , test "Incorrect Response" <|
            \() ->
                Expect.equal
                    [ { id = Nothing
                      , sessionId = "sessionId"
                      , sort = 0
                      , fixation = Nothing
                      , selection = Just 1495254367455
                      , pictures = Just 1495254366705
                      , redcross = Just 1495254367706
                      , probe = Nothing
                      , border = Just 1495254366809
                      , timeout = Nothing
                      , rest = Nothing
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
                    ]
                    (Game.Cycle.generate "sessionId"
                        [ EndTrial 1495254368205
                        , BeginDisplay (Just (RedCross Gray)) 1495254367706
                        , AcceptIndication { desired = False } 1495254367455
                        , BeginDisplay (Just (Single Gray { url = "http://localhost:8655/repo/8e78d083cf59100fb48529c0a55505e5.png", id = "1479458643823499512" })) 1495254366809
                        , BeginInput 1495254366809
                        , BeginDisplay (Just (Single None { url = "http://localhost:8654/repo/8e78d083cf59100fb48529c0a55505e5.png", id = "1479458643823499512" })) 1495254366705
                        , BeginTrial 1495254366705
                        ]
                    )
        , test "Incorrect Timeout" <|
            \() ->
                Expect.equal
                    [ { id = Nothing
                      , sessionId = "sessionId"
                      , sort = 0
                      , fixation = Nothing
                      , selection = Nothing
                      , pictures = Just 1495254548968
                      , redcross = Just 1495254549976
                      , probe = Nothing
                      , border = Just 1495254549077
                      , timeout = Just 1495254549976
                      , rest = Nothing
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
                    ]
                    (Game.Cycle.generate "sessionId"
                        [ EndTrial 1495254550472
                        , BeginDisplay (Just (RedCross Blue)) 1495254549976
                        , Timeout { desired = False } 1495254549976
                        , BeginDisplay (Just (Single Blue { url = "http://localhost:8654/repo/b08bf9ffc308d4cef0870283ae2bfbf3.png", id = "7169127603820459510" })) 1495254549077
                        , BeginInput 1495254549077
                        , BeginDisplay (Just (Single None { url = "http://localhost:8654/repo/b08bf9ffc308d4cef0870283ae2bfbf3.png", id = "7169127603820459510" })) 1495254548968
                        , BeginTrial 1495254548968
                        ]
                    )
        ]
