module Main exposing (..)

import Api
import Browser.Events
import Empty exposing (initialModel)
import Game
import Helpers
import Keyboard
import Model as M
import Port
import Routing as R
import Task
import Update
import View
import Window


main : Program M.Flags M.Model M.Msg
main =
    Browser.application M.OnUpdateLocation
        { init = init
        , view = View.view
        , update = Update.update
        , subscriptions = subscriptions
        }

subscriptions : M.Model -> Sub M.Msg
subscriptions model =
    Sub.batch
        [ Keyboard.downs M.Presses
        , ticker model.gameState
        , Port.status M.SetStatus
        , Port.domLoaded M.DomLoaded
        , Window.resizes M.WindowResize
        ]


ticker : Game.GameState M.Msg -> Sub M.Msg
ticker gameState =
    if Game.isPlaying gameState then
        Browser.Events.onAnimationFrame M.NewCurrentTime

    else
        Sub.none


init : M.Flags -> Url.Url -> Browser.Navigation.Key -> ( M.Model, Cmd M.Msg )
init flags location key =
    let
        ( httpsrv, tasksrv, filesrv ) =
            servers location.host

        -- based on location and jwt
        ( route_, visitor_, commands_ ) =
            case Api.okyToky flags.time flags.token of
                Ok jwt ->
                    ( Helpers.checkAccess (R.parseLocation location) jwt
                    , M.LoggedIn jwt
                    , Api.fetchAll httpsrv jwt flags.token
                    )

                Err _ ->
                    ( R.LoginRoute
                    , M.Anon
                    , Browser.Navigation.pushUrl key R.loginPath
                    )

        model_ =
            { initialModel
                | httpsrv = httpsrv
                , tasksrv = tasksrv
                , filesrv = filesrv
                , jwtencoded = flags.token
                , activeRoute = route_
                , visitor = visitor_
                , loadTime = flags.time
                , key = key
            }
    in
    Api.fetchFmriUserData model_
        |> Tuple.mapSecond
            (\cmd ->
                Cmd.batch
                    [ commands_
                    , cmd
                    , Task.perform M.WindowResize Window.size
                    ]
            )


servers : String -> ( String, String, String )
servers hostname =
    case hostname of
        "localhost" ->
            ( "http://localhost:8680"
            , "http://localhost:8668"
            , "http://localhost:8654"
            )

        "127.0.0.1" ->
            ( "http://localhost:8680"
            , "http://localhost:8668"
            , "http://localhost:8654"
            )

        _ ->
            ( "https://api.cravecontrol.org"
            , "https://task.cravecontrol.org"
            , "https://file.cravecontrol.org"
            )
