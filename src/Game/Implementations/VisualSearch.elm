module Game.Implementations.VisualSearch exposing (init)

import Duration exposing (Duration)
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
        , onSelect
        , segment
        , selectTimeout
        , showZoom
        , startSession
        , timeout
        , timeoutFromSegmentStart
        )
import List.Extra
import Quantity
import Random exposing (Generator)
import Random.List
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
    , zoomDuration : Duration
    , totalBlocks : Int
    , restDuration : Duration
    , intervalMin : Duration
    , intervalJitter : Duration
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
    { fixationDuration : Duration
    , imageDuration : Duration
    , zoomDuration : Duration
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
            Random.step (Random.List.shuffle (goImage :: List.take 15 noGoImagesShuffled)) newSeed

        goIndex =
            case List.Extra.elemIndex goImage images of
                Just index ->
                    index

                Nothing ->
                    Debug.todo "goImage is not in the list for some reason"

        layout =
            Just (SelectGrid None { columns = 4, images = images, goIndex = goIndex })

        fixation =
            Just (Fixation None)
    in
    log BeginTrial { state | trialResult = Game.NoResult, trialStart = state.currTime, currentSeed = nextSeed }
        |> andThen (log (BeginDisplay fixation))
        |> andThen (segment [ timeout fixationDuration ] fixation)
        |> andThen (log (BeginDisplay layout))
        |> andThen (log BeginInput)
        |> andThen (segment [ onSelect goIndex, selectTimeout (Quantity.plus fixationDuration imageDuration) ] layout)
        |> andThen (segment [ timeoutFromSegmentStart zoomDuration ] layout)
        |> andThen (log EndTrial)
