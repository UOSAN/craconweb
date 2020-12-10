module Cycle.GoNoGoTests exposing (all)

import Expect
import Game exposing (BorderType(..), Direction(..), Layout(..), LogEntry(..))
import Game.Cycle
import Test exposing (..)


all : Test
all =
    describe "GoNoGo Tests"
        [ goTests
        , noGoTests
        ]


goTests : Test
goTests =
    describe "Go"
        [ test "Timeout Go" <|
            \() ->
                Expect.equal
                    [ { id = Nothing
                      , sessionId = "sessionId"
                      , sort = 0
                      , fixation = Nothing
                      , selection = Nothing
                      , pictures = Just 1495162244273
                      , redcross = Just 1495162245538
                      , probe = Nothing
                      , border = Just 1495162244273
                      , timeout = Just 1495162245538
                      , rest = Nothing
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
                    ]
                    (Game.Cycle.generate "sessionId"
                        [ EndTrial 1495162246038
                        , BeginDisplay (Just (RedCross Black)) 1495162245538
                        , Timeout { desired = False } 1495162245538
                        , BeginInput 1495162244273
                        , BeginDisplay (Just (LeftOrRight Black Right { url = "http://localhost:8654/repo/36ba69efa7c71b0430cb0a5ecfaff5be.png", id = "6920533513820449510" })) 1495162244273
                        , BeginTrial 1495162244273
                        ]
                    )
        , test "Correct Go" <|
            \() ->
                Expect.equal
                    [ { id = Nothing
                      , sessionId = "sessionId"
                      , sort = 0
                      , fixation = Nothing
                      , selection = Just 1495240100609
                      , pictures = Just 1495240100139
                      , redcross = Nothing
                      , probe = Nothing
                      , border = Just 1495240100139
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
                      , images = [ "2543813863820289509" ]
                      }
                    ]
                    (Game.Cycle.generate "sessionId"
                        [ EndTrial 1495240100609
                        , AcceptDirection { desired = Left, actual = Left } 1495240100609
                        , BeginInput 1495240100139
                        , BeginDisplay (Just (LeftOrRight Black Left { url = "http://localhost:8654/repo/e585413c320e34313f1bed18dba2945a.png", id = "2543813863820289509" })) 1495240100139
                        , BeginTrial 1495240100139
                        ]
                    )
        , test "Incorrect Go" <|
            \() ->
                Expect.equal
                    [ { id = Nothing
                      , sessionId = "sessionId"
                      , sort = 0
                      , fixation = Nothing
                      , selection = Just 1495248757557
                      , pictures = Just 1495248756701
                      , redcross = Just 1495248757557
                      , probe = Nothing
                      , border = Just 1495248756701
                      , timeout = Nothing
                      , rest = Nothing
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
                    ]
                    (Game.Cycle.generate "sessionId"
                        [ EndTrial 1495248758455
                        , BeginDisplay (Just (RedCross Black)) 1495248757557
                        , AcceptDirection { desired = Right, actual = Left } 1495248757557
                        , BeginInput 1495248756701
                        , BeginDisplay (Just (LeftOrRight Black Right { url = "http://localhost:8654/repo/e310af4d3a76f05b81b1e41e0921e8c5.png", id = "755506963820219512" })) 1495248756701
                        , BeginTrial 1495248756701
                        ]
                    )
        ]


noGoTests : Test
noGoTests =
    describe "NoGo"
        [ test "Timeout/Correct NoGo" <|
            \() ->
                Expect.equal
                    [ { id = Nothing
                      , sessionId = "sessionId"
                      , sort = 0
                      , fixation = Nothing
                      , selection = Nothing
                      , pictures = Just 1495249097925
                      , redcross = Nothing
                      , probe = Nothing
                      , border = Just 1495249097925
                      , timeout = Just 1495249099189
                      , rest = Nothing
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
                    ]
                    (Game.Cycle.generate "sessionId"
                        [ EndTrial 1495249099189
                        , Timeout { desired = True } 1495249099189
                        , BeginInput 1495249097925
                        , BeginDisplay (Just (LeftOrRight Dashed Right { url = "http://localhost:8654/repo/454574ac9a2d099147aa51be6a1b0c2e.png", id = "4615715483824159516" })) 1495249097925
                        , BeginTrial 1495249097925
                        ]
                    )
        , test "Incorrect NoGo" <|
            \() ->
                Expect.equal
                    [ { id = Nothing
                      , sessionId = "sessionId"
                      , sort = 0
                      , fixation = Nothing
                      , selection = Just 1495250615497
                      , pictures = Just 1495250615013
                      , redcross = Just 1495250615497
                      , probe = Nothing
                      , border = Just 1495250615013
                      , timeout = Nothing
                      , rest = Nothing
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
                    ]
                    (Game.Cycle.generate "sessionId"
                        [ EndTrial 1495250616779
                        , BeginDisplay (Just (RedCross Dashed)) 1495250615497
                        , AcceptDirection { desired = Right, actual = Right } 1495250615497
                        , BeginInput 1495250615013
                        , BeginDisplay (Just (LeftOrRight Dashed Right { url = "http://localhost:8654/repo/8c32e32fb6a060810758faa5af457351.png", id = "4260027113823189510" })) 1495250615013
                        , BeginTrial 1495250615013
                        ]
                    )
        ]
