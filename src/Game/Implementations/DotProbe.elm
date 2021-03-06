module Game.Implementations.DotProbe exposing (init)

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
        , log
        , onDirection
        , segment
        , timeout
        )
import Quantity
import Random
import Time


init :
    { fixationDuration : Duration
    , imageDuration : Duration
    , infoString : String
    , responseImages : List Image
    , nonResponseImages : List Image
    , seedInt : Int
    , currentTime : Time.Posix
    , blockDuration : Duration
    , restDuration : Duration
    , totalBlocks : Int
    , intervalMin : Duration
    , intervalJitter : Duration
    }
    -> ( Game msg, Random.Seed )
init ({ fixationDuration, imageDuration, responseImages, nonResponseImages } as args) =
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
    { fixationDuration : Duration
    , imageDuration : Duration
    , goTrial : Bool
    , goImage : Image
    , noGoImage : Image
    }
    -> State
    -> Game msg
trial { fixationDuration, imageDuration, goImage, noGoImage } state =
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

        layout =
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
        |> andThen (log (BeginDisplay layout))
        |> andThen (segment [ timeout (Quantity.plus fixationDuration imageDuration) ] layout)
        |> andThen (log (BeginDisplay probe))
        |> andThen (log BeginInput)
        |> andThen (segment [ onDirection True direction ] probe)
        |> andThen (log EndTrial)
