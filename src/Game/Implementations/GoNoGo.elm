module Game.Implementations.GoNoGo exposing (init)

import Game
    exposing
        ( BorderType(..)
        , Game
        , Image
        , Layout(..)
        , LogEntry(..)
        , State
        , addIntervals
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
    { totalDuration : Time
    , infoString : String
    , responseImages : List Image
    , nonResponseImages : List Image
    , fillerImages : List Image
    , seedInt : Int
    , currentTime : Time
    , blockDuration : Time
    , redCrossDuration : Time
    , totalBlocks : Int
    , restDuration : Time
    , intervalMin : Time
    , intervalJitter : Time
    }
    -> ( Game msg, Random.Seed )
init ({ totalDuration, infoString, responseImages, nonResponseImages, fillerImages, seedInt, currentTime, redCrossDuration } as args) =
    let
        gos =
            responseImages
                |> List.map
                    (trial
                        { totalDuration = totalDuration
                        , goTrial = True
                        , redCrossDuration = redCrossDuration
                        }
                    )

        noGos =
            nonResponseImages
                |> List.map
                    (trial
                        { totalDuration = totalDuration
                        , goTrial = False
                        , redCrossDuration = redCrossDuration
                        }
                    )

        fillers =
            fillerImages
                |> List.map
                    (trial
                        { totalDuration = totalDuration
                        , goTrial = True
                        , redCrossDuration = redCrossDuration
                        }
                    )

        trials =
            gos ++ noGos ++ fillers
    in
    Game.shuffle args trials


trial :
    { totalDuration : Time
    , redCrossDuration : Time
    , goTrial : Bool
    }
    -> Image
    -> State
    -> Game msg
trial { totalDuration, goTrial, redCrossDuration } image state =
    let
        ( direction, nextSeed ) =
            Random.step Game.leftOrRight state.currentSeed

        borderType =
            if goTrial then
                Black

            else
                Dashed

        bordered =
            Just (LeftOrRight borderType direction image)

        redCross =
            Just (RedCross borderType)
    in
    log BeginTrial { state | trialResult = Game.NoResult, trialStart = state.currTime, currentSeed = nextSeed }
        |> andThen (log (BeginDisplay bordered))
        |> andThen (log BeginInput)
        |> andThen
            (segment
                [ onDirection goTrial direction
                , resultTimeout (not goTrial) totalDuration
                ]
                bordered
            )
        |> andThen (logWithCondition isFailed (BeginDisplay redCross))
        |> andThen (segment [ trialFailed, timeoutFromSegmentStart redCrossDuration ] redCross)
        |> andThen (log EndTrial)
