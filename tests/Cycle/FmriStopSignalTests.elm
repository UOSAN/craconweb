module Cycle.FmriStopSignalTests exposing (all)

import Expect
import Game exposing (BorderType(..), Direction(..), Layout(..), LogEntry(..))
import Game.Cycle
import Test exposing (..)


all : Test
all =
    describe "FmriStopSignal Tests"
        [ test "Correct Response" <|
            \() ->
                Expect.equal
                    [ { id = Nothing
                      , sessionId = "sessionId"
                      , sort = 0
                      , fixation = Nothing
                      , selection = Nothing
                      , pictures = Just 1498361908394
                      , redcross = Nothing
                      , probe = Nothing
                      , border = Just 1498361908494
                      , timeout = Just 1498361909395
                      , rest = Just 1498361920394
                      , interval = Just 1498361909395
                      , width = Nothing
                      , height = Nothing
                      , blue = False
                      , gray = True
                      , dash = False
                      , targetIndex = 0
                      , selectedIndex = 0
                      , startIndex = 0
                      , images = [ "210374515808349668" ]
                      }
                    ]
                    (Game.Cycle.generate "sessionId"
                        [ BeginDisplay (Just Rest) 1498361920394
                        , BeginDisplay (Just Interval) 1498361916027
                        , BeginDisplay (Just Interval) 1498361916027
                        , BeginDisplay (Just Interval) 1498361909395
                        , EndTrial 1498361909395
                        , Timeout { desired = True } 1498361909395
                        , BeginDisplay (Just (Single Gray { url = "http://localhost:8654/repo/df01a1fc573101cf57ce3a9bdc4d15ac.png", id = "210374515808349668" })) 1498361908494
                        , BeginInput 1498361908494
                        , BeginDisplay (Just (Single None { url = "http://localhost:8654/repo/df01a1fc573101cf57ce3a9bdc4d15ac.png", id = "210374515808349668" })) 1498361908394
                        , BeginTrial 1498361908394
                        , BeginDisplay (Just Interval) 1498361904260
                        ]
                    )
        ]
