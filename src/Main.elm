module Main exposing (..)

import Api
import Browser.Events
import Game
import Helpers
import Model as M
import Port
import Routing as R
import Task
import Update
import View
import Model exposing (WindowSize)
import Time


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
        [ Browser.Events.onKeyDown Helpers.keyDecoder M.Presses
        , ticker model.gameState
        , Port.status M.SetStatus
        , Port.domLoaded M.DomLoaded
        , Browser.Events.onResize (\w h -> M.WindowResize (getWindowSize w h))
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
            case Api.okyToky (Time.millisToPosix flags.time) flags.token of
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
            { httpsrv = httpsrv
            , tasksrv = tasksrv
            , filesrv = filesrv
            , jwtencoded = flags.token
            , activeRoute = route_
            , visitor = visitor_
            , isMenuActive = False
            , user = Nothing
            , login = { username = "", password = "" }
            , ugimages_v = Nothing
            , ugimages_i = Nothing
            , ugimages_f = Nothing
            , loading = Nothing
            , glitching = Nothing
            , informing = Nothing
            , users = []
            , userRole = emptyRole
            , groupIdExp = Nothing
            , groupIdCon = Nothing
            , httpErr = ""
            , gonogoGame = Nothing
            , dotprobeGame = Nothing
            , stopsignalGame = Nothing
            , respondsignalGame = Nothing
            , visualsearchGame = Nothing
            , gameState = Game.NotPlaying
            , ugimgsets = Nothing
            , mesQuery = Nothing
            , mesQuerys = Nothing
            , mesAnswers = Nothing
            , mesAnswer = Nothing
            , adminModel =
                { tmpUserRecord = Empty.emptyUserRecord
                , mesAnswers = Nothing
                , tmpUserEdit = Nothing
                }
            , statements = Nothing
            , request = Nothing
            , loadTime = Time.millisToPosix flags.time
            , badgeRules = RemoteData.NotAsked
            , domLoaded = False
            , badgesEarned = RemoteData.NotAsked
            , fmriUserData = RemoteData.NotAsked
            , statementsModal = False
            , windowSize = Nothing
            , key = key
            }

    in
    Api.fetchFmriUserData model_
        |> Tuple.mapSecond
            (\cmd ->
                Cmd.batch
                    [ commands_
                    , cmd
                    ]
            )


getWindowSize : Int -> Int -> WindowSize
getWindowSize w h =
    {width = w, height = h}


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
