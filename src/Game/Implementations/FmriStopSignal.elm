module Game.Implementations.FmriStopSignal exposing (init)

import Duration exposing (Duration)
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
import Random exposing (Generator)
import Time


init :
    { borderDelay : Duration
    , totalDuration : Duration
    , infoString : String
    , responseImages : List Image
    , nonResponseImages : List Image
    , seedInt : Int
    , currentTime : Time.Posix
    , redCrossDuration : Duration
    , blockDuration : Duration
    , restDuration : Duration
    , totalBlocks : Int
    , intervalMin : Duration
    , intervalJitter : Duration
    }
    -> ( Game msg, Random.Seed )
init ({ borderDelay, totalDuration, infoString, responseImages, nonResponseImages, seedInt, currentTime, blockDuration, redCrossDuration, totalBlocks, restDuration } as args) =
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
    in
    Game.shuffle args trials


trial :
    { borderDelay : Duration
    , totalDuration : Duration
    , blockDuration : Duration
    , redCrossDuration : Duration
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
