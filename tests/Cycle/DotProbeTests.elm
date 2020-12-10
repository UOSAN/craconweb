module Cycle.DotProbeTests exposing (all)

import Expect
import Game exposing (BorderType(..), Direction(..), Layout(..), LogEntry(..))
import Game.Cycle
import Test exposing (..)


all : Test
all =
    describe "DotProbe Tests"
        [ test "Incorrect" <|
            \() ->
                Expect.equal
                    [ { id = Nothing
                      , sessionId = "sessionId"
                      , sort = 0
                      , fixation = Just 1495251474320
                      , selection = Just 1495251476011
                      , pictures = Just 1495251474839
                      , redcross = Nothing
                      , probe = Just 1495251475338
                      , border = Nothing
                      , timeout = Nothing
                      , rest = Nothing
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
                    ]
                    (Game.Cycle.generate "sessionId"
                        [ EndTrial 1495251476011
                        , AcceptDirection { desired = Left, actual = Right } 1495251476011
                        , BeginInput 1495251475338
                        , BeginDisplay (Just (Probe None Left)) 1495251475338
                        , BeginDisplay (Just (LeftRight None Left { url = "http://localhost:8654/repo/b08bf9ffc308d4cef0870283ae2bfbf3.png", id = "7169127603820459510" } { url = "http://localhost:8654/repo/1bca12920c7453bf421b288e300c6bd3.png", id = "7245635463824259514" })) 1495251474839
                        , BeginDisplay (Just (Fixation None)) 1495251474320
                        , BeginTrial 1495251474320
                        ]
                    )
        , test "Correct" <|
            \() ->
                Expect.equal
                    [ { id = Nothing
                      , sessionId = "sessionId"
                      , sort = 0
                      , fixation = Just 1495252338828
                      , selection = Just 1495252340664
                      , pictures = Just 1495252339330
                      , redcross = Nothing
                      , probe = Just 1495252339828
                      , border = Nothing
                      , timeout = Nothing
                      , rest = Nothing
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
                    ]
                    (Game.Cycle.generate "sessionId"
                        [ EndTrial 1495252340664
                        , AcceptDirection { desired = Left, actual = Left } 1495252340664
                        , BeginInput 1495252339828
                        , BeginDisplay (Just (Probe None Left)) 1495252339828
                        , BeginDisplay (Just (LeftRight None Left { url = "http://localhost:8654/repo/efb6e164b03834f45bb6095f0a1a6f91.png", id = "7679994453820479507" } { url = "http://localhost:8654/repo/c6c0581db9b519b754a14998fda08b02.png", id = "7754139733824279513" })) 1495252339330
                        , BeginDisplay (Just (Fixation None)) 1495252338828
                        , BeginTrial 1495252338828
                        ]
                    )
        ]
