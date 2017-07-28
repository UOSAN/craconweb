module Game.Implementations.FmriStopSignal
    exposing
        ( init
        )

import Game
    exposing
        ( BorderType(..)
        , Game
        , Image
        , Layout(..)
        , LogEntry(..)
        , State
        , andThen
        , andThenRest
        , andThenCheckTimeout
        , emptyState
        , info
        , isFailed
        , leftOrRight
        , log
        , logWithCondition
        , onDirection
        , onIndication
        , resultTimeout
        , segment
        , startSession
        , timeout
        , timeoutFromSegmentStart
        , trialFailed
        )
import Game.Card exposing (complete)
import Random exposing (Generator)
import Random.List
import Time exposing (Time)


init :
    { borderDelay : Time
    , totalDuration : Time
    , infoString : String
    , responseImages : List Image
    , nonResponseImages : List Image
    , seedInt : Int
    , currentTime : Time
    , blockDuration : Time
    , restDuration : Time
    , totalBlocks : Int
    , redCrossDuration : Time
    }
    -> Game msg
init { borderDelay, totalDuration, infoString, responseImages, nonResponseImages, seedInt, currentTime, blockDuration, redCrossDuration, totalBlocks, restDuration } =
    let
        gos =
            responseImages
                |> List.map
                    (trial
                        { borderDelay = borderDelay
                        , totalDuration = totalDuration
                        , goTrial = True
                        , blockDuration = blockDuration
                        , redCrossDuration = redCrossDuration
                        }
                    )

        noGos =
            nonResponseImages
                |> List.map
                    (trial
                        { borderDelay = borderDelay
                        , totalDuration = totalDuration
                        , goTrial = False
                        , blockDuration = blockDuration
                        , redCrossDuration = redCrossDuration
                        }
                    )

        trials =
            gos ++ noGos

        shouldRest state =
            state.blockStart
                |> Maybe.map (\blockStart -> blockStart + blockDuration < state.currTime)
                |> Maybe.withDefault False

        isFinish state =
            state.blockCounter + 1 >= totalBlocks

        addIntervals =
            Game.addIntervals Nothing (3 * Time.second) (4 * Time.second)
    in
        Random.List.shuffle trials
            |> Random.andThen addIntervals
            |> Random.map
                (\trials ->
                    (startSession :: log (BeginSession { seed = seedInt }) :: trials)
                        |> List.foldl
                            (andThenRest
                                { restDuration = restDuration
                                , shouldRest = shouldRest
                                , isFinish = isFinish
                                }
                            )
                            (Game.Card.complete (emptyState seedInt currentTime))
                )
            |> (\generator -> Random.step generator (Random.initialSeed seedInt))
            |> Tuple.first


trial :
    { borderDelay : Time
    , totalDuration : Time
    , blockDuration : Time
    , redCrossDuration : Time
    , goTrial : Bool
    }
    -> Image
    -> State
    -> Game msg
trial { borderDelay, totalDuration, goTrial, blockDuration, redCrossDuration } image state =
    let
        borderType =
            if goTrial then
                Blue
            else
                Gray

        borderless =
            Just (Single None image)

        bordered =
            Just (Single borderType image)

        redCross =
            Just (RedCross borderType)
    in
        log BeginTrial { state | trialResult = Game.NoResult, trialStart = state.currTime }
            |> andThen (log (BeginDisplay borderless))
            |> andThen (segment [ timeout borderDelay ] borderless)
            |> andThen (log BeginInput)
            |> andThen (log (BeginDisplay bordered))
            |> andThen (segment [ onIndication goTrial, resultTimeout (not goTrial) totalDuration ] bordered)
            |> andThen (logWithCondition isFailed (BeginDisplay redCross))
            |> andThen (segment [ trialFailed, timeoutFromSegmentStart redCrossDuration ] redCross)
            |> andThen (log EndTrial)
