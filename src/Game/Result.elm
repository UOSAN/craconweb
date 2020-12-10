module Game.Result exposing
    ( averageResponseTimeInMillisecond
    , isCorrect
    , percentCorrect
    )

import Game
import Game.Cycle
import Maybe.Extra exposing (isJust, isNothing)


averageResponseTimeInMillisecond : Game.State -> Result String Float
averageResponseTimeInMillisecond state =
    let
        f log ( maybeStartTime, responseTimes ) =
            case maybeStartTime of
                Nothing ->
                    case log of
                        Game.BeginInput timestamp ->
                            ( Just timestamp, responseTimes )

                        _ ->
                            ( Nothing, responseTimes )

                Just beginTime ->
                    case log of
                        Game.AcceptIndication _ responseTime ->
                            ( Nothing, (responseTime - beginTime) :: responseTimes )

                        Game.AcceptDirection _ responseTime ->
                            ( Nothing, (responseTime - beginTime) :: responseTimes )

                        Game.AcceptSelection _ responseTime ->
                            ( Nothing, (responseTime - beginTime) :: responseTimes )

                        Game.EndTrial _ ->
                            ( Nothing, responseTimes )

                        _ ->
                            ( Just beginTime, responseTimes )

        responseTimes =
            state.log
                |> List.foldr f ( Nothing, [] )
                |> Tuple.second

        totalResponses =
            responseTimes |> List.length
    in
    if totalResponses == 0 then
        Err "No Response"

    else
        Ok <| List.sum responseTimes / toFloat totalResponses


percentCorrect : { gameSlug : String } -> Game.State -> Float
percentCorrect gameSlug state =
    let
        cycles =
            state.log |> Game.Cycle.generate ""

        totalAnswer =
            List.length cycles

        correctAnswers =
            cycles
                |> List.filter (isCorrect gameSlug)
                |> List.length
    in
    (toFloat correctAnswers / toFloat totalAnswer) * 100


isCorrect : { gameSlug : String } -> Game.Cycle -> Bool
isCorrect { gameSlug } cycle =
    case gameSlug of
        "dotprobe" ->
            isDotProbeCorrect cycle

        "gonogo" ->
            isGoNoGoCorrect cycle

        "stopsignal" ->
            isStopSignalCorrect cycle

        "visualsearch" ->
            isVisualSearchCorrect cycle

        _ ->
            False


isDotProbeCorrect : Game.Cycle -> Bool
isDotProbeCorrect cycle =
    cycle.targetIndex == cycle.selectedIndex


isGoNoGoCorrect : Game.Cycle -> Bool
isGoNoGoCorrect cycle =
    if not cycle.dash then
        isNothing cycle.timeout && (cycle.selectedIndex == cycle.targetIndex)

    else
        isJust cycle.timeout


isStopSignalCorrect : Game.Cycle -> Bool
isStopSignalCorrect cycle =
    if cycle.blue then
        isNothing cycle.timeout

    else
        isJust cycle.timeout


isVisualSearchCorrect : Game.Cycle -> Bool
isVisualSearchCorrect cycle =
    isNothing cycle.timeout && (cycle.targetIndex == cycle.selectedIndex)
