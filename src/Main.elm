module Main exposing (..)

import Api
import Browser.Events
import Empty exposing (initialModel)
import Game
import Helpers
import Keyboard
import Model as M
import Navigation
import Port
import Routing as R
import Task
import Update
import View
import Window


main : Program M.Flags M.Model M.Msg
main =
    Navigation.programWithFlags M.OnUpdateLocation
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


init : M.Flags -> Navigation.Location -> ( M.Model, Cmd M.Msg )
init flags location =
    let
        ( httpsrv, tasksrv, filesrv ) =
            servers location.hostname

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
                    , Navigation.newUrl R.loginPath
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
