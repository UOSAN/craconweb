module Main exposing (..)

import Model exposing (..)
import Update exposing (..)
import View exposing (..)
import Navigation


main : Program Never Model Msg
main =
    Navigation.program UrlChange
        { init = init
        , view = view
        , update = update
        , subscriptions = (\m -> Sub.none)
        }


init : Navigation.Location -> ( Model, Cmd Msg )
init location =
    ( model location, Cmd.none )
