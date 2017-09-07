module Game.Implementations.VisualSearch exposing (init)

import Game
    exposing
        ( Game
        , Image
        , Layout(..)
        , BorderType(..)
        , LogEntry(..)
        , State
        , onDirection
        , andThen
        , andThenCheckTimeout
        , emptyState
        , segment
        , log
        , addIntervals
        , info
        , timeout
        , timeoutFromSegmentStart
        , selectTimeout
        , startSession
        , leftOrRight
        , onSelect
        , showZoom
        )
import Random exposing (Generator)
import Random.List
import Time exposing (Time)
import List.Extra


init :
    { fixationDuration : Time
    , imageDuration : Time
    , infoString : String
    , responseImages : List Image
    , nonResponseImages : List Image
    , seedInt : Int
    , currentTime : Time
    , blockDuration : Time
    , zoomDuration : Time
    , totalBlocks : Int
    , restDuration : Time
    , intervalMin : Time
    , intervalJitter : Time
    }
    -> ( Game msg, Random.Seed )
init ({ fixationDuration, imageDuration, zoomDuration, infoString, responseImages, nonResponseImages, seedInt, currentTime, blockDuration } as args) =
    let
        trials =
            responseImages
                |> List.map
                    (trial
                        { fixationDuration = fixationDuration
                        , imageDuration = imageDuration
                        , zoomDuration = zoomDuration
                        , goTrial = True
                        , noGoImages = nonResponseImages
                        }
                    )
    in
        Game.shuffle args trials


trial :
    { fixationDuration : Time
    , imageDuration : Time
    , zoomDuration : Time
    , goTrial : Bool
    , noGoImages : List Image
    }
    -> Image
    -> State
    -> Game msg
trial { fixationDuration, imageDuration, zoomDuration, goTrial, noGoImages } goImage state =
    let
        ( noGoImagesShuffled, newSeed ) =
            Random.step (Random.List.shuffle noGoImages) state.currentSeed

        ( images, nextSeed ) =
            Random.step (Random.List.shuffle (goImage :: (List.take 15 noGoImagesShuffled))) newSeed

        goIndex =
            case List.Extra.elemIndex goImage images of
                Just index ->
                    index

                Nothing ->
                    Debug.crash "goImage is not in the list for some reason"

        trial =
            Just (SelectGrid None { columns = 4, images = images, goIndex = goIndex })

        fixation =
            Just (Fixation None)
    in
        log BeginTrial { state | trialResult = Game.NoResult, trialStart = state.currTime, currentSeed = nextSeed }
            |> andThen (log (BeginDisplay fixation))
            |> andThen (segment [ timeout fixationDuration ] fixation)
            |> andThen (log (BeginDisplay trial))
            |> andThen (log BeginInput)
            |> andThen (segment [ onSelect goIndex, selectTimeout (fixationDuration + imageDuration) ] trial)
            |> andThen (segment [ timeoutFromSegmentStart zoomDuration ] trial)
            |> andThen (log EndTrial)
