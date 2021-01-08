module Ui.Parts exposing
    ( grid
    , linkAttrs
    , middleBlock
    , modal
    , modalCard
    , notification
    )

import Helpers
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import List.Extra
import Model exposing (Msg(..))
import RemoteData
import Routing as R


notification : Maybe String -> String -> Html Msg
notification notifText mods =
    case notifText of
        Just nTxt ->
            div
                [ class <| "notification " ++ mods ]
                [ button [ class "delete", onClick ResetNotifications ] []
                , text nTxt
                ]

        Nothing ->
            text ""


linkAttrs : String -> List (Attribute Msg)
linkAttrs path =
    [ href <| path, R.onLinkClick <| UpdateLocation path ]


modal : List (Html msg) -> Html msg
modal children =
    div
        [ class "modal is-active" ]
        [ div
            [ class "modal-background" ]
            []
        , div
            [ class "modal-content" ]
            children
        ]


modalCard : msg -> String -> List (Html msg) -> Html msg
modalCard msg title children =
    div
        [ class "modal is-active" ]
        [ div
            [ class "modal-background" ]
            []
        , div
            [ class "modal-card" ]
            [ header
                [ class "modal-card-head" ]
                [ p
                    [ class "modal-card-title" ]
                    [ text title ]
                , button [ class "delete", onClick msg ] []
                ]
            , section
                [ class "modal-card-body" ]
                children
            , footer [ class "modal-card-foot" ] []
            ]
        ]


grid : Int -> List (Html Msg) -> List (Html Msg)
grid num children =
    List.map column children
        |> List.Extra.greedyGroupsOf num
        |> List.map (div [ class "columns" ])


column : Html Msg -> Html Msg
column col =
    div [ class "column" ]
        [ col ]


middleBlock : List (Html msg) -> Html msg
middleBlock children =
    div
        [ class "columns" ]
        [ div
            [ class "column is-6 is-offset-3" ]
            [ div
                [ class "card" ]
                [ div
                    [ class "card-content" ]
                    children
                ]
            ]
        ]
