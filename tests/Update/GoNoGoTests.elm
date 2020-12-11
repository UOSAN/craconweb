module Update.GoNoGoTests exposing (all)

import Date
import Duration exposing (Duration)
import Empty exposing (initialModel)
import Entity
import Expect
import Game
import Game.Cycle as Cycle
import Game.Implementations.GoNoGo as GoNoGo
import Model exposing (Msg(..))
import Protobuf
import Random
import Test exposing (..)
import Time
import Update


all : Test
all =
    describe "Go/No Go Game"
        [ currentTimeShouldBeUpdated
        , shouldTimedout
        , answerBeforeTimeout
        , shouldHaveSelection
        ]


currentTimeShouldBeUpdated : Test
currentTimeShouldBeUpdated =
    test "Current Time should be updated" <|
        \() ->
            let
                timestamp =
                    0

                msgs =
                    [ InitGoNoGo
                    , StartSession
                        { gameId = (game timestamp).id
                        , game = gameStateData timestamp
                        , time = timestamp
                        , initialSeed = round timestamp
                        , nextSeed = nextSeed timestamp
                        }
                    , NewCurrentTime (Duration.addTo timestamp (Duration.milliseconds 10))
                    , DirectionInput Game.Left
                    ]
            in
            Expect.equal
                (Just (Duration.addTo timestamp (Duration.milliseconds 10))
                (List.foldl (\msg model -> Update.update msg model |> Tuple.first) (goNoGoModel timestamp) msgs |> toTime)


shouldTimedout : Test
shouldTimedout =
    test "Game should timed out" <|
        \() ->
            let
                timestamp =
                    0

                msgs =
                    [ InitGoNoGo
                    , StartSession
                        { gameId = (game timestamp).id
                        , game = gameStateData timestamp
                        , time = timestamp
                        , initialSeed = round timestamp
                        , nextSeed = nextSeed timestamp
                        }
                    , NewCurrentTime (Duration.addTo timestamp (Duration.milliseconds 0))
                    , NewCurrentTime (Duration.addTo timestamp (Duration.milliseconds 1250))
                    , DirectionInput Game.Left
                    ]
            in
            Expect.equal
                [ { id = Nothing
                  , sessionId = "SessionId"
                  , sort = 0
                  , fixation = Nothing
                  , selection = Nothing
                  , pictures = Just timestamp
                  , redcross = Nothing
                  , probe = Nothing
                  , border = Just timestamp
                  , timeout = Just (Duration.addTo timestamp (Duration.milliseconds 1250))
                  , rest = Nothing
                  , interval = Just (Duration.addTo timestamp (Duration.milliseconds 1250))
                  , width = Just 2
                  , height = Nothing
                  , blue = False
                  , gray = False
                  , dash = True
                  , targetIndex = 0
                  , selectedIndex = 0
                  , startIndex = 0
                  , images = [ "non-response" ]
                  }
                ]
                (List.foldl (\msg model -> Update.update msg model |> Tuple.first) (goNoGoModel timestamp) msgs |> toCycles)


shouldHaveSelection : Test
shouldHaveSelection =
    test "Game should have selection" <|
        \() ->
            let
                timestamp =
                    1000

                msgs =
                    [ InitGoNoGo
                    , StartSession
                        { gameId = (game timestamp).id
                        , game = gameStateData timestamp
                        , time = timestamp
                        , initialSeed = round timestamp
                        , nextSeed = nextSeed timestamp
                        }
                    , NewCurrentTime (Duration.addTo timestamp (Duration.milliseconds 0))
                    , NewCurrentTime (Duration.addTo timestamp (Duration.milliseconds 1249))
                    , DirectionInput Game.Left
                    , NewCurrentTime (Duration.addTo timestamp (Duration.milliseconds 1250))
                    ]
            in
            Expect.equal
                [ { id = Nothing
                  , sessionId = "SessionId"
                  , sort = 0
                  , fixation = Nothing
                  , selection = Just (Duration.addTo timestamp (Duration.milliseconds 1249))
                  , pictures = Just timestamp
                  , redcross = Nothing
                  , probe = Nothing
                  , border = Just timestamp
                  , timeout = Nothing
                  , rest = Nothing
                  , interval = Just (Duration.addTo timestamp (Duration.milliseconds 1249))
                  , width = Just 2
                  , height = Nothing
                  , blue = False
                  , gray = False
                  , dash = False
                  , targetIndex = 0
                  , selectedIndex = 0
                  , startIndex = 0
                  , images = [ "filler" ]
                  }
                ]
                (List.foldl (\msg model -> Update.update msg model |> Tuple.first) (goNoGoModel timestamp) msgs |> toCycles)


answerBeforeTimeout : Test
answerBeforeTimeout =
    let
        msgs timestamp =
            [ InitGoNoGo
            , StartSession
                { gameId = (game timestamp).id
                , game = gameStateData timestamp
                , time = timestamp
                , initialSeed = round timestamp
                , nextSeed = nextSeed timestamp
                }
            , NewCurrentTime (Duration.addTo timestamp (Duration.milliseconds 0))
            , NewCurrentTime (Duration.addTo timestamp (Duration.milliseconds 1249))
            , DirectionInput Game.Left
            , NewCurrentTime (Duration.addTo timestamp (Duration.milliseconds 1250))
            , NewCurrentTime (Duration.addTo timestamp (Duration.milliseconds 1251))
            ]
    in
    describe "Answer before timeout"
        [ test "Wrong answer" <|
            \() ->
                let
                    timestamp =
                        0
                in
                Expect.equal
                    [ { id = Nothing
                      , sessionId = "SessionId"
                      , sort = 0
                      , fixation = Nothing
                      , selection = Just (Duration.addTo timestamp (Duration.milliseconds 1249))
                      , pictures = Just timestamp
                      , redcross = Just (Duration.addTo timestamp (Duration.milliseconds 1249))
                      , probe = Nothing
                      , border = Just timestamp
                      , timeout = Nothing
                      , rest = Nothing
                      , interval = Nothing
                      , width = Just 2
                      , height = Nothing
                      , blue = False
                      , gray = False
                      , dash = True
                      , targetIndex = 0
                      , selectedIndex = 0
                      , startIndex = 0
                      , images = [ "non-response" ]
                      }
                    ]
                    (List.foldl (\msg model -> Update.update msg model |> Tuple.first) (goNoGoModel timestamp) (msgs timestamp) |> toCycles)
        , test "Right answer" <|
            \() ->
                let
                    timestamp =
                        3
                in
                Expect.equal
                    [ { id = Nothing
                      , sessionId = "SessionId"
                      , sort = 0
                      , fixation = Nothing
                      , selection = Just (Duration.addTo timestamp (Duration.milliseconds 1249))
                      , pictures = Just timestamp
                      , redcross = Nothing
                      , probe = Nothing
                      , border = Just timestamp
                      , timeout = Nothing
                      , rest = Nothing
                      , interval = Just (Duration.addTo timestamp (Duration.milliseconds 1249))
                      , width = Just 2
                      , height = Nothing
                      , blue = False
                      , gray = False
                      , dash = False
                      , targetIndex = 0
                      , selectedIndex = 0
                      , startIndex = 0
                      , images = [ "response" ]
                      }
                    ]
                    (List.foldl (\msg model -> Update.update msg model |> Tuple.first) (goNoGoModel timestamp) (msgs timestamp) |> toCycles)
        ]



-- HELPERS


toLogEntries : Model.Model -> List Game.LogEntry
toLogEntries model =
    model
        |> toState
        |> Maybe.map .log
        |> Maybe.withDefault []


toCycles : Model.Model -> List Game.Cycle
toCycles model =
    model
        |> toLogEntries
        |> Cycle.generate "SessionId"


toState : Model.Model -> Maybe Game.State
toState model =
    case model.gameState of
        Game.Playing { game } ->
            Just (Game.unwrap game)

        _ ->
            Nothing


protobufTimestamp : Time.Posix -> Protobuf.Timestamp
protobufTimestamp timestamp =
    Date.fromTime timestamp


game : Time.Posix -> Entity.Game
game timestamp =
    { id = "TEST"
    , name = "Go No-Go"
    , slug = "gonogo"
    , dscript = ""
    , instruct = ""
    , icon = "gonogo.png"
    , reactDur = 500
    , sessDur = 300000
    , trialDur = 1250
    , offsetDur = 0
    , fixDur = 500
    , fixImg = "x.png"
    , durInc = 0
    , durDec = 0
    , incTrigger = 0
    , decTrigger = 0
    , blocked = Nothing
    , created = Just (protobufTimestamp timestamp)
    , updated = Just (protobufTimestamp timestamp)
    , deleted = Nothing
    }


initialSeed : Time.Posix -> Random.Seed
initialSeed timestamp =
    Random.initialSeed Time.posixToMillis timestamp


responseImage : Game.Image
responseImage =
    { id = "response"
    , url = "response"
    }


nonResponseImage : Game.Image
nonResponseImage =
    { id = "non-response"
    , url = "non-response"
    }


fillerImage : Game.Image
fillerImage =
    { id = "filler"
    , url = "filler"
    }


gameStateData : Time.Posix -> Game.Game Msg
gameStateData timestamp =
    generateGameStateData timestamp |> Tuple.first


nextSeed : Time.Posix -> Random.Seed
nextSeed timestamp =
    generateGameStateData timestamp |> Tuple.second


generateGameStateData : Time.Posix -> ( Game.Game Msg, Random.Seed )
generateGameStateData timestamp =
    GoNoGo.init
        { totalDuration = Duration.milliseconds 1250
        , infoString = """
<h3 class="title">Instructions</h3>
<p>You will see pictures either on the left or right side of the screen, surrounded by a solid or dashed border. Press <span class="highlight"><strong>c</strong></span> when the picture is on the left side of the screen or <span class="highlight"><strong>m</strong></span> when the picture is on the right side of the screen. BUT only if you see a <span style="border: 1px solid rgb(0, 0, 0); padding: 2px;">solid border</span> around the picture. Do not press if you see a <span style="border: 1px dashed rgb(0, 0, 0); padding: 2px;">dashed border</span>. Go as fast as you can, but don't sacrifice accuracy for speed.<div>
<br>
<br>
<strong>Press any key or tap here to continue.</strong></div>
</p>
"""
        , responseImages = List.range 1 10 |> List.map (always responseImage)
        , nonResponseImages = List.range 1 10 |> List.map (always nonResponseImage)
        , fillerImages = List.range 1 10 |> List.map (always fillerImage)
        , seedInt = Time.posixToMillis timestamp
        , currentTime = timestamp
        , redCrossDuration = Duration.milliseconds 500
        , blockDuration = Duration.minutes 1
        , totalBlocks = 5
        , restDuration = Duration.seconds 10
        , intervalMin = Duration.milliseconds 500
        , intervalJitter = Duration.milliseconds 0
        }


gameSession : Time.Posix -> Game.Session
gameSession timestamp =
    { id = ""
    , userId = ""
    , gameId = ""
    , seed = Time.posixToMillis timestamp
    , start = timestamp
    , end = Nothing
    , jitter = False
    }


goNoGoModel : Time.Posix -> Model.Model
goNoGoModel timestamp =
    { initialModel
        | gonogoGame = Just (game timestamp)
        , gameState =
            Game.Playing
                { game = gameStateData timestamp
                , session = gameSession timestamp
                , nextSeed = nextSeed timestamp
                }
    }


debugLogEntries : Model.Model -> Model.Model
debugLogEntries model =
    let
        _ =
            Debug.log "LogEntry" (model |> toLogEntries)
    in
    model


debugCycles : Model.Model -> Model.Model
debugCycles model =
    let
        _ =
            Debug.log "Cycles" (model |> toCycles)
    in
    model


debugTime : Model.Model -> Model.Model
debugTime model =
    let
        _ =
            Debug.log "time" (model |> toTime)
    in
    model


toTime : Model.Model -> Maybe Time.Posix
toTime model =
    model |> toState |> Maybe.map .currTime
