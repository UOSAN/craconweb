module Game exposing (..)

import Duration exposing (Duration)
import Game.Card as Card exposing (Continuation(..))
import Random exposing (Generator)
import Random.Extra
import Random.List
import RemoteData
import Time


type GameState msg
    = NotPlaying
    | Loading (Game msg) (RemoteData.WebData Session)
    | Playing { game : Game msg, session : Session, nextSeed : Random.Seed }
    | Saving State Session (RemoteData.WebData ( Session, List Cycle ))
    | Saved State { session : Session, cycles : List Cycle }


type alias Session =
    { id : String
    , userId : String
    , gameId : String
    , seed : Int
    , start : Time.Posix
    , end : Maybe Time.Posix
    , jitter : Bool
    }


type alias Cycle =
    { id : Maybe String
    , sessionId : String
    , sort : Int
    , fixation : Maybe Time.Posix
    , selection : Maybe Time.Posix
    , pictures : Maybe Time.Posix
    , redcross : Maybe Time.Posix
    , probe : Maybe Time.Posix
    , border : Maybe Time.Posix
    , timeout : Maybe Time.Posix
    , rest : Maybe Time.Posix
    , interval : Maybe Time.Posix
    , width : Maybe Int
    , height : Maybe Int
    , blue : Bool
    , gray : Bool
    , dash : Bool
    , targetIndex : Int
    , selectedIndex : Int
    , startIndex : Int
    , images : List String
    }


type alias Game msg =
    Card.Card State Layout Input msg


type alias Continuation msg =
    Card.Continuation State Layout Input msg


type Input
    = Initialize
    | Tick Time.Posix
    | Indication
    | Select Int
    | Direction Direction


type Direction
    = Left
    | Right


flipDirection : Direction -> Direction
flipDirection direction =
    case direction of
        Left ->
            Right

        Right ->
            Left


directionToIndex : Direction -> Int
directionToIndex direction =
    case direction of
        Left ->
            0

        Right ->
            1


type Layout
    = Info BorderType String
    | Single BorderType Image
    | LeftOrRight BorderType Direction Image
    | LeftRight BorderType Direction Image Image
    | SelectGrid BorderType { columns : Int, images : List Image, goIndex : Int }
    | RedCross BorderType
    | Fixation BorderType
    | Probe BorderType Direction
    | Interval
    | Rest


type BorderType
    = None
    | Gray
    | Blue
    | Black
    | Dashed


type alias Image =
    { id : String
    , url : String
    }


type LogEntry
    = BeginSession { seed : Int } Time.Posix
    | EndSession Time.Posix
    | BeginTrial Time.Posix
    | EndTrial Time.Posix
    | BeginDisplay (Maybe Layout) Time.Posix
    | BeginInput Time.Posix
    | AcceptIndication { desired : Bool } Time.Posix
    | AcceptDirection { desired : Direction, actual : Direction } Time.Posix
    | AcceptSelection { desired : Int, actual : Int } Time.Posix
    | Timeout { desired : Bool } Time.Posix


type alias State =
    { sessionStart : Maybe Time.Posix
    , blockStart : Maybe Time.Posix
    , trialStart : Time.Posix
    , segmentStart : Time.Posix
    , currTime : Time.Posix
    , log : List LogEntry
    , trialResult : Result
    , currentSeed : Random.Seed
    , blockCounter : Int
    }


type Result
    = NoResult
    | BoolResult Bool
    | SelectResult { result : Bool, answer : Maybe Int }


type alias Logic =
    State -> Input -> ( Bool, State )


unwrap : Game msg -> State
unwrap =
    Card.unwrap Initialize


segment : List Logic -> Maybe Layout -> State -> Game msg
segment logics layout state =
    let
        combined =
            oneOf (updateCurrTime :: logics) state
    in
    Card.card
        layout
        (\input ->
            case combined input of
                ( True, newState ) ->
                    ( Continue newState (segment logics layout newState), Cmd.none )

                ( False, newState ) ->
                    ( Complete newState, Cmd.none )
        )


andThenCheckTimeout : Duration -> (State -> Game msg) -> Game msg -> Game msg
andThenCheckTimeout gameDuration =
    Card.andThen (isTimeout gameDuration) resetSegmentStart Initialize


isTimeout : Duration -> State -> Bool
isTimeout gameDuration state =
    state.sessionStart
        |> Maybe.map (\sessionStart -> Duration.addTo sessionStart gameDuration < state.currTime)
        |> Maybe.withDefault False


andThenRest : { restDuration : Duration, shouldRest : State -> Bool, isFinish : State -> Bool } -> (State -> Game msg) -> Game msg -> Game msg
andThenRest { restDuration, shouldRest, isFinish } =
    Card.andThenRest
        { restCard = rest restDuration
        , restDuration = restDuration
        , shouldRest = shouldRest
        , isFinish = isFinish
        , isInterval = isInterval
        , resetSegmentStart = resetSegmentStart
        , resetBlockStart = resetBlockStart
        , initialize = Initialize
        }


isInterval : Game msg -> Bool
isInterval game =
    case Card.layout game of
        Just Interval ->
            True

        _ ->
            False


andThen : (State -> Game msg) -> Game msg -> Game msg
andThen =
    Card.andThen (always False) resetSegmentStart Initialize


resetSegmentStart : State -> State
resetSegmentStart state =
    { state | segmentStart = state.currTime }


resetBlockStart : Duration -> State -> State
resetBlockStart restDuration state =
    { state
        | blockStart = Just (Duration.addTo state.currTime restDuration)
        , blockCounter = state.blockCounter + 1
    }


oneOf : List Logic -> Logic
oneOf logics state input =
    List.foldl
        (\logic ( continue, newState ) ->
            if continue then
                logic newState input

            else
                ( continue, newState )
        )
        ( True, state )
        logics


log : (Time.Posix -> LogEntry) -> State -> Game msg
log =
    logWithCondition (always True)


logWithCondition : (State -> Bool) -> (Time.Posix -> LogEntry) -> State -> Game msg
logWithCondition enabled logEntry originalState =
    Card.card
        Nothing
        (\input ->
            let
                ( _, updatedState ) =
                    updateCurrTime originalState input
            in
            ( Complete
                (if enabled updatedState then
                    { updatedState | log = logEntry updatedState.currTime :: updatedState.log }

                 else
                    updatedState
                )
            , Cmd.none
            )
        )


rest : Duration -> State -> Game msg
rest duration state =
    log (BeginDisplay (Just Rest)) (startTrial state)
        |> andThen (segment [ timeoutFromSegmentStart duration ] (Just Rest))


interval : Duration -> State -> Game msg
interval expiration state =
    log (BeginDisplay (Just Interval)) (startTrial state)
        |> andThen (segment [ timeout expiration ] (Just Interval))


randomInterval : Duration -> Duration -> Generator (State -> Game msg)
randomInterval min jitter =
    Random.float Duration.inMilliseconds min (Duration.inMilliseconds (Duration.addTo min jitter))
        |> Random.map interval


addIntervals : Maybe Layout -> Duration -> Duration -> List (State -> Game msg) -> Generator (List (State -> Game msg))
addIntervals layout min jitter trials =
    trials
        |> List.map Random.Extra.constant
        |> List.intersperse (randomInterval min jitter)
        |> Random.Extra.combine


prependInterval : Maybe Layout -> Duration -> Duration -> List (State -> Game msg) -> Generator (List (State -> Game msg))
prependInterval layout min jitter trials =
    randomInterval min jitter
        :: List.map Random.Extra.constant trials
        |> Random.Extra.combine


startSession : State -> Game msg
startSession state =
    Card.complete
        { state
            | sessionStart = Just state.currTime
            , blockStart = Just state.currTime
        }


startTrial : State -> State
startTrial state =
    { state | trialStart = state.currTime, trialResult = NoResult }


info : String -> State -> Game msg
info infoString state =
    segment [ advanceOnIndication ] (Just (Info None infoString)) state


advanceOnIndication : Logic
advanceOnIndication state input =
    case input of
        Indication ->
            ( False, state )

        _ ->
            ( True, state )


onIndication : Bool -> Logic
onIndication desired state input =
    case ( input, state.trialResult ) of
        ( Indication, NoResult ) ->
            ( False
            , { state
                | log = AcceptIndication { desired = desired } state.currTime :: state.log
                , trialResult = BoolResult desired
              }
            )

        _ ->
            ( True, state )


onSelect : Int -> Logic
onSelect desiredIndex state input =
    case ( input, state.trialResult ) of
        ( Select actualIndex, NoResult ) ->
            ( False
            , { state
                | log = AcceptSelection { desired = desiredIndex, actual = actualIndex } state.currTime :: state.log
                , trialResult = SelectResult { result = desiredIndex == actualIndex, answer = Just actualIndex }
              }
            )

        _ ->
            ( True, state )


onDirection : Bool -> Direction -> Logic
onDirection desired desiredDirection state input =
    case ( input, state.trialResult ) of
        ( Direction actualDirection, NoResult ) ->
            ( False
            , { state
                | log = AcceptDirection { desired = desiredDirection, actual = actualDirection } state.currTime :: state.log
                , trialResult = BoolResult (desired && desiredDirection == actualDirection)
              }
            )

        _ ->
            ( True, state )


timeout : Duration -> Logic
timeout expiration state _ =
    ( Duration.addTo state.trialStart expiration > state.currTime, state )


timeoutFromSegmentStart : Duration -> Logic
timeoutFromSegmentStart expiration state _ =
    ( Duration.addTo state.segmentStart expiration > state.currTime, state )


selectTimeout : Duration -> Logic
selectTimeout expiration state input =
    case ( state.trialResult, timeout expiration state input ) of
        ( NoResult, ( False, newState ) ) ->
            ( False
            , { state
                | log = Timeout { desired = False } state.currTime :: newState.log
                , trialResult = SelectResult { result = False, answer = Nothing }
              }
            )

        ( BoolResult _, ( False, newState ) ) ->
            ( False, newState )

        ( SelectResult _, ( False, newState ) ) ->
            ( False, newState )

        ( _, ( True, newState ) ) ->
            ( True, newState )


resultTimeout : Bool -> Duration -> Logic
resultTimeout desired expiration state input =
    case ( state.trialResult, timeout expiration state input ) of
        ( NoResult, ( False, newState ) ) ->
            ( False
            , { state
                | log = Timeout { desired = desired } state.currTime :: newState.log
                , trialResult = BoolResult desired
              }
            )

        ( BoolResult _, ( False, newState ) ) ->
            ( False, newState )

        ( SelectResult _, ( False, newState ) ) ->
            ( False, newState )

        ( _, ( True, newState ) ) ->
            ( True, newState )


isFailed : State -> Bool
isFailed state =
    case state.trialResult of
        NoResult ->
            False

        BoolResult True ->
            False

        BoolResult False ->
            True

        SelectResult { result } ->
            not result


trialFailed : Logic
trialFailed state input =
    state
        |> isFailed
        |> (\a -> (\a b -> ( a, b )) a state)


showZoom : Logic
showZoom state input =
    case state.trialResult of
        NoResult ->
            ( False, state )

        BoolResult True ->
            ( False, state )

        BoolResult False ->
            ( False, state )

        SelectResult _ ->
            ( True, state )


updateCurrTime : Logic
updateCurrTime state input =
    case input of
        Tick time ->
            ( True, { state | currTime = time } )

        _ ->
            ( True, state )


emptyState : Int -> Time.Posix -> State
emptyState initialSeed time =
    { sessionStart = Nothing
    , blockStart = Nothing
    , trialStart = time
    , segmentStart = time
    , currTime = time
    , log = []
    , trialResult = NoResult
    , currentSeed = Random.initialSeed initialSeed
    , blockCounter = 0
    }


isPlaying : GameState msg -> Bool
isPlaying gameState =
    case gameState of
        Playing _ ->
            True

        Loading _ _ ->
            False

        NotPlaying ->
            False

        Saving _ _ _ ->
            False

        Saved _ _ ->
            False


leftOrRight : Generator Direction
leftOrRight =
    Random.bool
        |> Random.map
            (\bool ->
                if bool then
                    Left

                else
                    Right
            )


shouldRest : Duration -> State -> Bool
shouldRest blockDuration state =
    state.blockStart
        |> Maybe.map (\blockStart -> (Duration.addTo blockStart blockDuration) < state.currTime)
        |> Maybe.withDefault False


isFinish : Int -> State -> Bool
isFinish totalBlocks state =
    state.blockCounter + 1 >= totalBlocks


restart : { totalBlocks : Int, blockDuration : Duration, restDuration : Duration, nextTrials : Generator (List (State -> Game msg)) } -> State -> GameState msg -> GameState msg
restart args state gameState =
    case gameState of
        Playing { game, session, nextSeed } ->
            let
                ( newGame, newSeed ) =
                    args.nextTrials
                        |> Random.map
                            (\trials ->
                                (trials ++ [ Card.restart args ])
                                    |> List.foldl
                                        (andThenRest
                                            { restDuration = args.restDuration
                                            , isFinish = isFinish args.totalBlocks
                                            , shouldRest = shouldRest args.blockDuration
                                            }
                                        )
                                        (Card.complete state)
                            )
                        |> (\generator -> Random.step generator nextSeed)
            in
            Playing
                { game = newGame
                , session = session
                , nextSeed = newSeed
                }

        Loading _ _ ->
            gameState

        NotPlaying ->
            gameState

        Saving _ _ _ ->
            gameState

        Saved _ _ ->
            gameState


shuffle : { a | blockDuration : Duration, currentTime : Time.Posix, intervalJitter : Duration, intervalMin : Duration, restDuration : Duration, seedInt : Int, totalBlocks : Int } -> List (State -> Game msg) -> ( Game msg, Random.Seed )
shuffle { seedInt, totalBlocks, blockDuration, restDuration, currentTime, intervalMin, intervalJitter } trials =
    Random.List.shuffle trials
        |> Random.andThen (addIntervals Nothing intervalMin intervalJitter)
        |> Random.map
            (\shuffledTrials ->
                (startSession
                    :: log (BeginSession { seed = seedInt })
                    :: (shuffledTrials
                            ++ [ Card.restart
                                    { totalBlocks = totalBlocks
                                    , blockDuration = blockDuration
                                    , restDuration = restDuration
                                    , nextTrials =
                                        trials
                                            |> Random.List.shuffle
                                            |> Random.andThen (addIntervals Nothing intervalMin intervalJitter)
                                            |> Random.andThen (prependInterval Nothing intervalMin intervalJitter)
                                    }
                               ]
                       )
                )
                    |> List.foldl
                        (andThenRest
                            { restDuration = restDuration
                            , shouldRest = shouldRest blockDuration
                            , isFinish = isFinish totalBlocks
                            }
                        )
                        (Card.complete (emptyState seedInt currentTime))
            )
        |> (\generator -> Random.step generator (Random.initialSeed seedInt))
