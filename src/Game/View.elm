module Game.View exposing (view, viewResult)

import Game exposing (BorderType(..))
import Game.Card
import Game.Result
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Markdown
import Ui.Card
import Numeral


view : Game.GameState msg -> msg -> Html msg
view gameState msg =
    case gameState of
        Game.Playing game ->
            let
                state =
                    game |> Game.unwrap

                timer =
                    state.sessionStart
                        |> Maybe.map (\sessionStart -> state.currTime - sessionStart)
                        |> Maybe.map (\timer -> timer / 1000)
                        |> Maybe.map toString
                        |> Maybe.withDefault ""
            in
                div []
                    -- [ p [] [ text timer ]
                    -- , p [] [ text <| toString state.trialResult ]
                    [ case Game.Card.layout game of
                        Nothing ->
                            text ""

                        Just (Game.Info borderType string) ->
                            Ui.Card.middleBlock [ Markdown.toHtml [] string ]

                        Just (Game.Single borderType image) ->
                            border borderType [ img [ src image.url ] [] ]

                        Just (Game.LeftRight borderType lImage rImage) ->
                            border borderType [ img [ src lImage.url ] [], img [ src rImage.url ] [] ]

                        Just (Game.LeftOrRight borderType direction image) ->
                            border borderType
                                [ img
                                    [ src image.url
                                    , case direction of
                                        Game.Left ->
                                            class "is-pulled-left squeezed"

                                        Game.Right ->
                                            class "is-pulled-right squeezed"
                                    ]
                                    []
                                ]

                        Just (Game.SelectGrid borderType rows cols images) ->
                            border borderType [ text (toString (rows * cols)) ]

                        Just (Game.RedCross borderType) ->
                            border borderType [ redCross ]
                    ]

        Game.Finished state ->
            viewResult (Just state)
                { percentCorrect = Game.Result.percentCorrect state
                , averageResponseTimeResult = Game.Result.averageResponseTimeInMillisecond state
                }

        Game.NotPlaying ->
            div []
                [ a
                    [ class "button is-info is-large"
                    , onClick msg
                    ]
                    [ text "Start Game" ]
                ]


viewResult : Maybe Game.State -> { a | percentCorrect : Float, averageResponseTimeResult : Result String Float } -> Html msg
viewResult state { percentCorrect, averageResponseTimeResult } =
    let
        averageResponseTime =
            case averageResponseTimeResult of
                Err error ->
                    error

                Ok result ->
                    Numeral.format "0.00" result ++ " milliseconds"
    in
        Ui.Card.middleBlock
            [ h1 [ class "title" ] [ text "Results" ]
            , ul []
                [ li [] [ text <| "Average Response Time: " ++ averageResponseTime ]
                , li [] [ text <| "Percent Correct: " ++ Numeral.format "0.00" percentCorrect ++ "%" ]
                ]
            ]


border : BorderType -> List (Html msg) -> Html msg
border borderType content =
    case borderType of
        None ->
            div [ class "gameWrapper" ] [ div [ class "imageBox sized" ] content ]

        Grey ->
            div [ class "gameWrapper" ] [ div [ class "imageBox greyBorder sized" ] content ]

        Blue ->
            div [ class "gameWrapper" ] [ div [ class "imageBox blueBorder sized" ] content ]

        Black ->
            div [ class "gameWrapper" ] [ div [ class "imageBox solidBorder sized" ] content ]

        Dashed ->
            div [ class "gameWrapper" ] [ div [ class "imageBox dashedBorder sized" ] content ]


redCross : Html msg
redCross =
    div [ class "redCross" ] [ text "X" ]
