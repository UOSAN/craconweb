module Game.Implementations.DotProbe exposing (init)

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
        , leftOrRight
        , log
        , onDirection
        , onIndication
        , resultTimeout
        , segment
        , startSession
        , timeout
        )
import Random exposing (Generator)
import Time exposing (Time)


init :
    { fixationDuration : Time
    , imageDuration : Time
    , infoString : String
    , responseImages : List Image
    , nonResponseImages : List Image
    , seedInt : Int
    , currentTime : Time
    , blockDuration : Time
    , restDuration : Time
    , totalBlocks : Int
    , intervalMin : Time
    , intervalJitter : Time
    }
    -> ( Game msg, Random.Seed )
init ({ fixationDuration, imageDuration, infoString, responseImages, nonResponseImages, seedInt, currentTime, blockDuration, restDuration, totalBlocks } as args) =
    let
        trials =
            List.map2
                (\goImage noGoImage ->
                    trial
                        { fixationDuration = fixationDuration
                        , imageDuration = imageDuration
                        , goTrial = True
                        , goImage = goImage
                        , noGoImage = noGoImage
                        }
                )
                responseImages
                nonResponseImages
    in
    Game.shuffle args trials


trial :
    { fixationDuration : Time
    , imageDuration : Time
    , goTrial : Bool
    , goImage : Image
    , noGoImage : Image
    }
    -> State
    -> Game msg
trial { fixationDuration, imageDuration, goTrial, goImage, noGoImage } state =
    let
        ( direction, firstSeed ) =
            Random.step Game.leftOrRight state.currentSeed

        probeDirectionGenerator =
            Random.float 0 1
                |> Random.map
                    (\n ->
                        if n < 0.9 then
                            direction

                        else
                            Game.flipDirection direction
                    )

        ( probeDirection, nextSeed ) =
            Random.step probeDirectionGenerator firstSeed

        borderless =
            None

        trial =
            case direction of
                Game.Left ->
                    Just (LeftRight borderless direction goImage noGoImage)

                Game.Right ->
                    Just (LeftRight borderless direction noGoImage goImage)

        fixation =
            Just (Fixation borderless)

        probe =
            Just (Probe borderless probeDirection)
    in
    log BeginTrial { state | trialResult = Game.NoResult, trialStart = state.currTime, currentSeed = nextSeed }
        |> andThen (log (BeginDisplay fixation))
        |> andThen (segment [ timeout fixationDuration ] fixation)
        |> andThen (log (BeginDisplay trial))
        |> andThen (segment [ timeout (fixationDuration + imageDuration) ] trial)
        |> andThen (log (BeginDisplay probe))
        |> andThen (log BeginInput)
        |> andThen (segment [ onDirection True direction ] probe)
        |> andThen (log EndTrial)
