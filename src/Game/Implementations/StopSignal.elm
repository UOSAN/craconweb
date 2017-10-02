module Game.Implementations.StopSignal
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
import Time exposing (Time)


init :
    { borderDelay : Time
    , totalDuration : Time
    , infoString : String
    , responseImages : List Image
    , nonResponseImages : List Image
    , seedInt : Int
    , currentTime : Time
    , redCrossDuration : Time
    , blockDuration : Time
    , restDuration : Time
    , totalBlocks : Int
    , intervalMin : Time
    , intervalJitter : Time
    }
    -> ( Game msg, Random.Seed )
init ({ borderDelay, totalDuration, infoString, responseImages, nonResponseImages, seedInt, currentTime, redCrossDuration } as args) =
    let
        gos =
            responseImages
                |> List.map
                    (trial
                        { borderDelay = borderDelay
                        , totalDuration = totalDuration
                        , goTrial = True
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
                        , redCrossDuration = redCrossDuration
                        }
                    )

        trials =
            gos ++ noGos
    in
        Game.shuffle args trials


trial :
    { borderDelay : Time
    , totalDuration : Time
    , redCrossDuration : Time
    , goTrial : Bool
    }
    -> Image
    -> State
    -> Game msg
trial { borderDelay, totalDuration, goTrial, redCrossDuration } image state =
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
