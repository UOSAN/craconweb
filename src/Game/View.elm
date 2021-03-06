module Game.View exposing (view, viewResult)

import Entity
import Game exposing (BorderType(..))
import Game.Card
import Game.Result
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http.Detailed
import Json.Decode
import List.Extra
import Markdown
import Model exposing (Msg(..))
import Numeral
import RemoteData
import Svg exposing (Svg, circle, svg)
import Svg.Attributes as Svg exposing (cx, cy, r)
import Ui.Parts
import Time


view :
    { gameState : Game.GameState Msg
    , initMsg : Msg
    , gameSlug : String
    , restMessages : List String
    , fmriUser : Maybe Entity.User
    }
    -> Html Msg
view { gameSlug, gameState, initMsg, fmriUser, restMessages } =
    case gameState of
        Game.Loading _ remoteData ->
            case remoteData of
                RemoteData.Loading ->
                    div []
                        [ a
                            [ class "button is-info is-large is-loading"
                            , onClick initMsg
                            ]
                            [ text "Start Game" ]
                        ]

                RemoteData.Failure _ ->
                    div []
                        [ a
                            [ class "button is-info is-large"
                            , onClick initMsg
                            ]
                            [ text "Try Again" ]
                        ]

                _ ->
                    text ""

        Game.Playing { game } ->
            let
                state =
                    game |> Game.unwrap

                _ =
                    state.sessionStart
                        |> Maybe.map (\sessionStart -> (Time.posixToMillis state.currTime) - (Time.posixToMillis sessionStart))
                        |> Maybe.map (\t -> toFloat t / 1000)
                        |> Maybe.map String.fromFloat
                        |> Maybe.withDefault ""
            in
            div []
                [ case Game.Card.layout game of
                    Nothing ->
                        text ""

                    Just (Game.Info _ string) ->
                        Ui.Parts.middleBlock [ Markdown.toHtml [ onClick IndicationInput ] string ]

                    Just (Game.Single borderType image) ->
                        viewSingleLayout borderType image

                    Just (Game.LeftRight borderType _ lImage rImage) ->
                        viewLeftRightLayout
                            { borderType = borderType
                            , lImage = lImage
                            , rImage = rImage
                            }

                    Just (Game.LeftOrRight borderType direction image) ->
                        viewLeftOrRightLayout
                            { borderType = borderType
                            , direction = direction
                            , image = image
                            }

                    Just (Game.SelectGrid borderType { columns, images, goIndex }) ->
                        viewSelectGridLayout
                            { borderType = borderType
                            , columns = columns
                            , images = images
                            , result = state.trialResult
                            , goIndex = goIndex
                            }

                    Just (Game.RedCross borderType) ->
                        viewRedCross borderType

                    Just (Game.Fixation borderType) ->
                        viewFixation borderType

                    Just (Game.Probe borderType direction) ->
                        viewProbe borderType direction

                    Just Game.Rest ->
                        viewRest restMessages state

                    Just Game.Interval ->
                        Html.text ""
                ]

        Game.Saving state session remoteData ->
            viewResult state
                session
                { percentCorrect = Game.Result.percentCorrect gameSlug state
                , averageResponseTimeResult = Game.Result.averageResponseTimeInMillisecond state
                , savingStatus = remoteData
                }

        Game.Saved state data ->
            viewResult state
                data.session
                { percentCorrect = Game.Result.percentCorrect gameSlug state
                , averageResponseTimeResult = Game.Result.averageResponseTimeInMillisecond state
                , savingStatus = RemoteData.Success data
                }

        Game.NotPlaying ->
            div []
                [ case fmriUser of
                    Nothing ->
                        div []
                            [ gameInstructions gameSlug
                            , a
                                [ class "button is-info is-large"
                                , onClick initMsg
                                ]
                                [ text "Start Game" ]
                            ]

                    Just user ->
                        div []
                            [ p [ class "" ]
                                [ text "fMRI for "
                                , strong [] [ text <| user.username ++ " (" ++ user.subject ++ ")" ]
                                , Ui.Parts.middleBlock
                                    [ h3 [ class "title is-3" ] [ text "Instructions" ]
                                    , p [] [ text """You will see pictures presented in either a dark blue or light gray border.
                                    Press the space bar as quickly as you can. BUT only if you see a blue border around the picture.
                                    Do not press if you see a gray border. Go as fast as you can, but don't sacrifice accuracy for speed.""" ]
                                    , p [] [ br [] [], strong [] [ text "Waiting for signal..." ] ]
                                    ]
                                ]
                            ]
                ]


viewResult : Game.State -> Game.Session -> { a | percentCorrect : Float, averageResponseTimeResult : Result String Float, savingStatus : RemoteData.RemoteData
              (Http.Detailed.Error String) b } -> Html Msg
viewResult state session { percentCorrect, averageResponseTimeResult, savingStatus } =
    let
        averageResponseTime =
            case averageResponseTimeResult of
                Err error ->
                    error

                Ok result ->
                    Numeral.format "0.00" result ++ " milliseconds"
    in
    Ui.Parts.middleBlock
        [ h1 [ class "title" ] [ text "Results" ]
        , ul []
            [ li [] [ text <| "Average Response Time: " ++ averageResponseTime ]
            , li [] [ text <| "Percent Correct: " ++ Numeral.format "0.00" percentCorrect ++ "%" ]
            ]
        , br [] []
        , case savingStatus of
            RemoteData.Failure _ ->
                button
                    [ class "button is-info is-large"
                    , onClick (ResendSession state session)
                    , type_ "button"
                    ]
                    [ text "Resend Data" ]

            RemoteData.Loading ->
                button [ class "button is-info is-large is-loading" ]
                    [ text "Resend Data" ]

            RemoteData.NotAsked ->
                text ""

            RemoteData.Success _ ->
                button
                    [ class "button is-info is-large"
                    , onClick (UpdateLocation "/")
                    , type_ "button"
                    ]
                    [ text "Done" ]
        ]


border : BorderType -> List (Attribute msg) -> List (Html msg) -> Html msg
border borderType attributes content =
    case borderType of
        None ->
            div (class "imageBox whiteBorder sized" :: attributes) content

        Gray ->
            div (class "imageBox grayBorder sized" :: attributes) content

        Blue ->
            div (class "imageBox blueBorder sized" :: attributes) content

        Black ->
            div (class "imageBox solidBorder sized" :: attributes) content

        Dashed ->
            div (class "imageBox dashedBorder sized" :: attributes) content


viewRedCross : BorderType -> Html msg
viewRedCross borderType =
    gameWrapper
        [ border borderType [] [ div [ class "redCross" ] [ text "X" ] ]
        ]


gameWrapper : List (Html msg) -> Html msg
gameWrapper game =
    div [ class "gameWrapper" ] game


viewSingleLayout : BorderType -> Game.Image -> Html Msg
viewSingleLayout borderType image =
    gameWrapper
        [ border borderType
            [ onTouch IndicationInput, onClick IndicationInput ]
            [ img
                [ src image.url
                , class "squeezed"
                , onTouch IndicationInput
                , onClick IndicationInput
                ]
                []
            ]
        ]


viewLeftOrRightLayout : { borderType : BorderType, direction : Game.Direction, image : Game.Image } -> Html Msg
viewLeftOrRightLayout { borderType, direction, image } =
    gameWrapper
        [ border borderType
            []
            [ div [ class "columns is-mobile" ]
                [ div [ class "column", onTouch (DirectionInput Game.Left), onClick (DirectionInput Game.Left) ]
                    [ case direction of
                        Game.Left ->
                            img [ src image.url ] []

                        Game.Right ->
                            text ""
                    ]
                , div [ class "column", onTouch (DirectionInput Game.Right), onClick (DirectionInput Game.Right) ]
                    [ case direction of
                        Game.Left ->
                            text ""

                        Game.Right ->
                            img [ src image.url ] []
                    ]
                ]
            ]
        ]



-- VISUAL SEARCH


viewSelectGridLayout : { result : Game.Result, borderType : BorderType, columns : Int, images : List Game.Image, goIndex : Int } -> Html Msg
viewSelectGridLayout { result, columns, images, goIndex } =
    div [ class "columns is-mobile" ]
        (List.Extra.groupsOf columns images
            |> List.indexedMap
                (\col tempImages ->
                    viewGridColumn
                        { result = result
                        , columnIndex = col
                        , images = tempImages
                        , goIndex = goIndex
                        }
                )
        )


viewGridColumn : { result : Game.Result, columnIndex : Int, images : List Game.Image, goIndex : Int } -> Html Msg
viewGridColumn { result, columnIndex, images, goIndex } =
    div [ class "column" ]
        (images
            |> List.indexedMap
                (viewGridRow
                    { result = result
                    , columnIndex = columnIndex
                    , goIndex = goIndex
                    }
                )
        )


viewGridRow : { result : Game.Result, columnIndex : Int, goIndex : Int } -> Int -> Game.Image -> Html Msg
viewGridRow { result, columnIndex, goIndex } rowIndex image =
    let
        index =
            (columnIndex * 4) + rowIndex
    in
    img
        [ src image.url
        , onClick (SelectInput index)
        , onTouch (SelectInput index)
        , case result of
            Game.SelectResult { answer } ->
                if goIndex == index then
                    class "vsImg green-grow"

                else
                    case Maybe.map ((==) index) answer of
                        Just True ->
                            class "vsImg red-shrink"

                        Just False ->
                            class "vsImg"

                        Nothing ->
                            class "vsImg"

            _ ->
                class "vsImg"
        ]
        []



-- DOT PROBE


viewLeftRightLayout : { borderType : BorderType, lImage : Game.Image, rImage : Game.Image } -> Html Msg
viewLeftRightLayout { lImage, rImage } =
    div [ class "columns is-mobile" ]
        [ div [ class "column", onTouch (DirectionInput Game.Left), onClick (DirectionInput Game.Left) ]
            [ img [ src lImage.url ] [] ]
        , div [ class "column", onTouch (DirectionInput Game.Right), onClick (DirectionInput Game.Right) ]
            [ img [ src rImage.url ] [] ]
        ]


viewFixation : BorderType -> Html msg
viewFixation _ =
    div
        [ class "columns is-mobile" ]
        [ div
            [ class "column" ]
            [ div [ class "fixationCross" ] [ text "+" ] ]
        ]


viewRest : List String -> Game.State -> Html Msg
viewRest messages state =
    let
        counter =
            state.blockStart
                |> Maybe.map (\blockStart -> ceiling (toFloat (Time.posixToMillis blockStart - Time.posixToMillis state.currTime) / 1000))
                |> Maybe.map (\countdown -> " in " ++ String.fromInt countdown ++ " seconds.")
                |> Maybe.withDefault "."

        defaultMessage =
            "This concludes Block "
                ++ String.fromInt state.blockCounter
                ++ ". Please stand by to begin Block "
                ++ String.fromInt (state.blockCounter + 1)
                ++ counter

        index =
            if List.length messages == 0 then
                0

            else
                modBy (List.length messages) (state.blockCounter - 1)

        message =
            messages |> List.Extra.getAt index |> Maybe.withDefault defaultMessage
    in
    Ui.Parts.middleBlock
        [ text <| message
        ]


viewProbe : BorderType -> Game.Direction -> Html Msg
viewProbe _ direction =
    div
        [ class "columns is-mobile" ]
        (case direction of
            Game.Left ->
                [ div
                    [ class "column", onTouch (DirectionInput Game.Left), onClick (DirectionInput Game.Left) ]
                    [ div [ class "probe" ] [ probe ] ]
                , div
                    [ class "column", onTouch (DirectionInput Game.Right), onClick (DirectionInput Game.Right) ]
                    [ text "" ]
                ]

            Game.Right ->
                [ div
                    [ class "column", onTouch (DirectionInput Game.Left), onClick (DirectionInput Game.Left) ]
                    [ text "" ]
                , div
                    [ class "column", onTouch (DirectionInput Game.Right), onClick (DirectionInput Game.Right) ]
                    [ div [ class "probe" ] [ probe ] ]
                ]
        )


probe : Svg msg
probe =
    svg [ Svg.width "20", Svg.height "20" ] [ circle [ cx "10", cy "10", r "10" ] [] ]


gameInstructions : String -> Html msg
gameInstructions gameSlug =
    case gameSlug of
        "gonogo" ->
            instBlock """You will see pictures either on
                                the left or right side of the screen, surrounded by a solid
                                or dashed border. Press 'c' when the picture is on the left
                                side of the screen or 'm' when the picture is on the right
                                side of the screen. BUT only if you see a solid bar around
                                the picture. Do not press if you see a dashed border. Go as
                                fast as you can, but don't sacrifice accuracy for speed."""

        "dotprobe" ->
            instBlock """You will see pictures on the
                    left and right side of the screen, followed by a dot on the
                    left or right side of the screen. Press the "c" if the dot is
                    on the left side of the screen or "m" when the dot is on the
                    right side of the screen. Go as fast as you can, but don't
                    sacrifice accuracy for speed."""

        "stopsignal" ->
            instBlock """You will see pictures presented
                     in either a dark blue or light gray border. Press the space
                      bar if you see a blue border around the picture.
                    Do not press if you see a gray border.
                        Go as fast as you can, but don't sacrifice accuracy for speed."""

        "visualsearch" ->
            instBlock """You will see a grid of images.
                    Select the target image as quickly as you can. Don't sacrifice
                    accuracy for speed."""

        _ ->
            text ""


instBlock : String -> Html msg
instBlock text_ =
    div [ class "box" ]
        [ p [] [ text text_ ] ]


onTouch : msg -> Html.Attribute msg
onTouch msg =
    let
        options =
            { message = msg
            , stopPropagation = True
            , preventDefault = True
            }
    in
    custom "touchstart" (Json.Decode.succeed options)
