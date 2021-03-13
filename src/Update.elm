module Update exposing (update)

import Api
import Browser.Navigation exposing (pushUrl, load)
import Duration
import Empty
import Entity
import Game
import Game.Card
import Game.Cycle
import Game.Implementations.DotProbe
import Game.Implementations.FmriStopSignal
import Game.Implementations.GoNoGo
import Game.Implementations.StopSignal
import Game.Implementations.VisualSearch
import Helpers
import Html exposing (..)
import Html.Attributes exposing (..)
import Http.Detailed
import Model exposing (..)
import Port
import Random
import RemoteData
import Routing as R
import Task
import Time
import Url
import Jwt


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        BadgesResp badges ->
            ( { model | badgesEarned = badges }
            , Cmd.none
            )

        BadgeRulesResp badgeRules_ ->
            ( { model | badgeRules = badgeRules_ }
            , Cmd.none
            )

        MesAnswersResp (Ok result) ->
            let
                cmd =
                    Task.attempt MesQuerysResp (Api.fetchMesQuerys { url = model.httpsrv, token = model.jwtencoded, sub = "" })
                myAnswers = result.body
            in
            ( { model | mesAnswers = Just myAnswers }, cmd )

        SetTmpUserEdit key value ->
            let
                adminModel_ =
                    model.adminModel

                tempUserEdit_ =
                    case model.adminModel.tmpUserEdit of
                        Nothing ->
                            Nothing

                        Just tue_ ->
                            case key of
                                "email" ->
                                    Just { tue_ | email = value }

                                "password" ->
                                    Just { tue_ | password = value }

                                "username" ->
                                    Just { tue_ | username = value }

                                "firstName" ->
                                    Just { tue_ | firstName = value }

                                "lastName" ->
                                    Just { tue_ | lastName = value }

                                _ ->
                                    Just tue_
            in
            ( { model | adminModel = { adminModel_ | tmpUserEdit = tempUserEdit_ } }
            , Cmd.none
            )

        ToggleTmpUserEditMesOptin ->
            let
                adminModel_ =
                    model.adminModel

                tempUserEdit_ =
                    case model.adminModel.tmpUserEdit of
                        Nothing ->
                            Nothing

                        Just tue_ ->
                            Just { tue_ | mesOptin = not tue_.mesOptin }
            in
            ( { model | adminModel = { adminModel_ | tmpUserEdit = tempUserEdit_ } }
            , Cmd.none
            )

        FillTmpUserEdit userId ->
            let
                user_ =
                    model.users
                        |> List.filter (\u -> u.id == userId)
                        |> List.head

                ( tmpUser, cmd ) =
                    case user_ of
                        Nothing ->
                            ( Nothing, Cmd.none )

                        Just u ->
                            ( Just
                                { id = u.id
                                , username = u.username
                                , firstName = u.firstName
                                , lastName = u.lastName
                                , email = u.email
                                , password = ""
                                , groupId = u.groupId
                                , mesOptin = u.mesOptin
                                }
                            , pushUrl model.key (R.editPath ++ u.id)
                            )
            in
            ( { model | adminModel = up_tmpUserEdit model.adminModel tmpUser }
            , cmd
            )

        SetRequestNothing ->
            ( { model | request = Nothing }, Cmd.none )

        MesQuerysResp (Ok result) ->
            let
                queries = result.body
                ( queryIds, _ ) =
                    case model.mesAnswers of
                        Nothing ->
                            ( [], Nothing )

                        Just mesAs ->
                            ( mesAs |> List.map .queryId
                            , List.head mesAs |> Maybe.map .created
                            )

                unanswered =
                    queries
                        |> List.filter (\q -> List.member q.id queryIds |> not)

                mesQuery_ =
                    List.head unanswered |> Maybe.map .content

                mesAnswer_ =
                    case List.head unanswered of
                        Nothing ->
                            Nothing

                        Just q ->
                            Just (newMesAnswerWithqueryId q.id)
            in
            ( { model
                | mesQuerys = Just unanswered
                , mesQuery = mesQuery_
                , mesAnswer = mesAnswer_
              }
            , Cmd.none
            )

        UpdateMesAnswer a ->
            ( { model | mesAnswer = Maybe.map (up_essay a) model.mesAnswer }, Cmd.none )

        TrySubmitMesAnswer ->
            case model.mesAnswer of
                Nothing ->
                    ( model
                    , Cmd.none
                    )

                Just mesAns ->
                    if mesAns.essay == "" then
                        ( { model | request = Just "Please answer the question. Thanks!" }, Cmd.none )

                    else if String.length mesAns.essay < 2 then
                        ( { model | request = Just "Maybe write a little more?" }, Cmd.none )

                    else
                        case model.visitor of
                            Anon ->
                                ( model
                                , Cmd.none
                                )

                            LoggedIn { sub } ->
                                ( { model | mesQuery = Nothing, request = Nothing }
                                , Task.attempt MesPostResp
                                    (Api.createMesAnswer
                                        { url = model.httpsrv
                                        , token = model.jwtencoded
                                        , sub = ""
                                        }
                                        mesAns
                                        sub
                                    )
                                )

        MesPostResp _ ->
            ( model
            , Cmd.none
            )

        GroupChanged groupId_ ->
            let
                ( adminModel, user, userEdit ) =
                    ( model.adminModel, model.adminModel.tmpUserRecord, model.adminModel.tmpUserEdit )

                user_ =
                    { user | groupId = Maybe.withDefault "" groupId_ }

                userEdit_ =
                    case userEdit of
                        Nothing ->
                            Nothing

                        Just u ->
                            Just { u | groupId = Maybe.withDefault "" groupId_ }

                adminModel_ =
                    { adminModel | tmpUserRecord = user_, tmpUserEdit = userEdit_ }
            in
            ( { model | adminModel = adminModel_ }, Cmd.none )

        UserEditResp (Ok _) ->
            ( { model | informing = Just "User saved." }
            , Cmd.none
            )

        UserEditResp (Err err) ->
            ( { model | informing = Just (Helpers.httpHumanError err) }
            , Cmd.none
            )

        TryPutUser ->
            case model.adminModel.tmpUserEdit of
                Nothing ->
                    ( model
                    , Cmd.none
                    )

                Just user ->
                    ( model
                    , Task.attempt UserEditResp
                        (Api.updateUser
                            { url = model.httpsrv
                            , token = model.jwtencoded
                            , sub = ""
                            }
                            user
                        )
                    )

        TryCsvUpload ->
            ( model
            , Port.upload
                ( model.tasksrv ++ "/upload/ugimgset"
                , "csvForm"
                , model.jwtencoded
                )
            )

        SetStatus response ->
            let
                resp =
                    errorCodeEncoder response

                remark =
                    if resp.error == "" then
                        "Thanks, upload and processing succeeded."

                    else
                        resp.error
            in
            ( { model | informing = Just remark }, Cmd.none )

        -- ADMIN
        GroupResp (Ok result) ->
            let
                group = result.body
            in
                case group.slug of
                    "control_a" ->
                        ( { model | groupIdCon = Just group.id }, Cmd.none )

                    "experimental_a" ->
                        ( { model | groupIdExp = Just group.id }, Cmd.none )

                    _ ->
                        ( model
                        , Cmd.none
                        )

        UsersResp (Ok result) ->
            let
                users_ = result.body
            in
                ( { model
                    | users = users_
                }
                , Cmd.none
                )

        SetRegistration key value ->
            let
                adminModel_ =
                    model.adminModel

                tmpUserRecord_old_ =
                    adminModel_.tmpUserRecord

                tmpUserRecord_old =
                    { tmpUserRecord_old_
                        | roles =
                            [ model.userRole.id ]
                    }

                tmpUserRecord_ =
                    case key of
                        "email" ->
                            { tmpUserRecord_old | email = value }

                        "password" ->
                            { tmpUserRecord_old | password = value }

                        "username" ->
                            { tmpUserRecord_old | username = value }

                        "firstName" ->
                            { tmpUserRecord_old | firstName = value }

                        "lastName" ->
                            { tmpUserRecord_old | lastName = value }

                        "exp" ->
                            { tmpUserRecord_old | groupId = value }

                        "con" ->
                            { tmpUserRecord_old | groupId = value }

                        _ ->
                            tmpUserRecord_old
            in
            ( { model | adminModel = { adminModel_ | tmpUserRecord = tmpUserRecord_ } }, Cmd.none )

        ToggleRegistrationMesOptin ->
            let
                adminModel_ =
                    model.adminModel

                tmpUserRecord_old =
                    adminModel_.tmpUserRecord

                tmpUserRecord_ =
                    { tmpUserRecord_old | mesOptin = not tmpUserRecord_old.mesOptin }
            in
            ( { model | adminModel = { adminModel_ | tmpUserRecord = tmpUserRecord_ } }, Cmd.none )

        EditUserAccount _ _ ->
            ( model
            , Cmd.none
            )

        TryRegisterUser ->
            if missing model.adminModel.tmpUserRecord then
                ( { model | glitching = Just "* Please fill out required fields." }, Cmd.none )

            else
                ( { model | loading = Just "loading..." }
                , Task.attempt RegisterUserResp
                    (Api.createUserRecord
                        model.httpsrv
                        model.jwtencoded
                        (fix_email model.adminModel.tmpUserRecord)
                    )
                )

        MesAuthorsResp (Ok result) ->
            let
                mesAuthors = result.body
            in
                case model.statements of
                    Nothing ->
                        ( model
                        , Cmd.none
                        )

                    Just mesAnswers ->
                        ( { model
                            | statements =
                                Just
                                    (List.map (up_mesAnswersDisplayName mesAuthors) mesAnswers)
                        }
                        , Cmd.none
                        )

        MesResp (Ok result) ->
            let
                mesAnswers = result.body
            in
                ( { model
                    | adminModel =
                        up_mesAnswers model.adminModel mesAnswers
                }
                , Cmd.none
                )

        PublicMesResp (Ok result) ->
            let
                publicMes = result.body
                cmd =
                    case model.visitor of
                        LoggedIn jwt ->
                            Api.fetchMesAuthors
                                { url = model.httpsrv
                                , token = model.jwtencoded
                                , sub = jwt.sub
                                }
                                publicMes
                                jwt.groupId
                                |> Task.attempt MesAuthorsResp

                        _ ->
                            Cmd.none
            in
            ( { model | statements = Just publicMes }, cmd )

        PublishMes id ->
            case model.adminModel.mesAnswers of
                Nothing ->
                    ( model
                    , Cmd.none
                    )

                Just mess ->
                    let
                        _ =
                            case model.visitor of
                                Anon ->
                                    ""

                                LoggedIn jwtdecoded ->
                                    jwtdecoded.sub

                        mesAns =
                            mess
                                |> List.filter (\m -> m.id == id)
                                |> List.head
                                |> Maybe.map (\m -> { m | public = True })

                        model_ =
                            case model.adminModel.mesAnswers of
                                Nothing ->
                                    model

                                Just mesA ->
                                    { model | adminModel = up_mesAnswers model.adminModel (List.filter (\m -> m.id /= id) mesA) }
                    in
                    case mesAns of
                        Nothing ->
                            ( model
                            , Cmd.none
                            )

                        Just mesAnswer ->
                            ( model_
                            , Task.attempt PutMesResp
                                (Api.updateMesStatus
                                    { url = model.httpsrv
                                    , token = model.jwtencoded
                                    , sub = mesAnswer.userId
                                    }
                                    id
                                    mesAnswer
                                )
                            )

        PutMesResp (Ok _) ->
            ( model
            , Cmd.none
            )

        RegisterUserResp (Ok result) ->
            let
                newUser = result.body
            in
                ( { model
                    | loading = Nothing
                    , users = ( newUser ) :: model.users
                    , adminModel =
                        up_tmpUserRecord model.adminModel Empty.emptyUserRecord
                }
                , pushUrl model.key R.adminPath
                )

        -- SHARED
        DomLoaded loaded ->
            ( { model | domLoaded = loaded }, Cmd.none )

        ResetNotifications ->
            ( { model
                | glitching = Nothing
                , informing = Nothing
                , loading = Nothing
              }
            , Cmd.none
            )

        UpdateLocation path ->
            ( model, pushUrl model.key path )

        OnUpdateLocation location ->
            onUpdateLocation location model

        -- LOGIN
        UpdateEmail username ->
            let
                login_ =
                    model.login
            in
            ( { model | login = { login_ | username = username } }
            , Cmd.none
            )

        UpdatePassword newPassword ->
            let
                login_ =
                    model.login
            in
            ( { model
                | login = { login_ | password = newPassword }
              }
            , Cmd.none
            )

        TryLogin ->
            let
                cmd =
                    Task.attempt AuthResp
                        (Api.createAuthRecord
                            model.httpsrv
                            model.login
                        )
            in
            ( { model | loading = Just "loading..." }, cmd )

        Logout ->
            let
                cmds =
                    [ Port.clear ()
                    , pushUrl model.key "/login"
                    ]
            in
            ( Empty.emptyModel model, Cmd.batch cmds )

        AuthResp (Ok result) ->
            let
                token = result.body
                jwtdecoded_ = Api.jwtDecoded token

                ( model_, command_ ) =
                    case jwtdecoded_ of
                        Ok jwt ->
                            let
                                isPowerful roles =
                                    List.member "staff" roles
                                        || List.member "admin" roles

                                skipToAdmin jsonWebTokenArg =
                                    if isPowerful (List.map .name jsonWebTokenArg.roles) then
                                        load R.adminPath

                                    else
                                        load R.homePath
                            in
                            ( { model
                                | loading = Nothing
                                , visitor = LoggedIn jwt
                                , jwtencoded = token
                                , glitching = Nothing
                              }
                            , Cmd.batch
                                [ Port.set ( "token", tokenEncoder token )
                                , Api.fetchAll model.httpsrv jwt token
                                , skipToAdmin jwt
                                ]
                            )

                        Err err ->
                            ( { model
                                | loading = Nothing
                                , glitching = Just (Jwt.errorToString err)
                              }
                            , Cmd.none
                            )
            in
            ( model_, command_ )

        UserResp (Ok result) ->
            let
                user_ = result.body
            in
                ( { model | user = Just user_ }
                , Cmd.none
                )

        StartSession data ->
            startSession data model

        StartSessionResp nextSeed game remoteData ->
            startSessionResp nextSeed game remoteData model

        GameDataSaved state session remoteData ->
            gameDataSaved state session remoteData model

        ResendSession state session ->
            saveGameData state session model

        -- TODO fetch configuration from the model
        InitStopSignal ->
            initStopSignal model

        InitFmriStopSignal _ ->
            initFmriStopSignal model

        FmriImagesResp result ->
            ( { model | fmriUserData = result }, Cmd.none )

        -- TODO fetch configuration from the model
        InitGoNoGo ->
            initGoNoGo model

        -- TODO fetch configuration from the model
        InitDotProbe ->
            initDotProbe model

        -- TODO fetch configuration from the model
        InitVisualSearch ->
            initVisualSearch model

        GameResp (Ok result) ->
            let
                game = result.body
            in
                case game.slug of
                    "gonogo" ->
                        ( { model | gonogoGame = Just game }, Cmd.none )

                    "dotprobe" ->
                        ( { model | dotprobeGame = Just game }, Cmd.none )

                    "stopsignal" ->
                        ( { model | stopsignalGame = Just game }, Cmd.none )

                    "respondsignal" ->
                        ( { model | respondsignalGame = Just game }, Cmd.none )

                    "visualsearch" ->
                        ( { model | visualsearchGame = Just game }, Cmd.none )

                    _ ->
                        ( model
                        , Cmd.none
                        )

        Presses keyCode ->
            presses keyCode model

        SelectInput n ->
            handleSelectInput n model

        DirectionInput direction ->
            handleDirectionInput direction model

        IndicationInput ->
            handleIndicationInput model

        MainMenuToggle ->
            ( { model | isMenuActive = not model.isMenuActive }, Cmd.none )

        NewCurrentTime t ->
            handleTimeUpdate t model

        RoleResp (Ok result) ->
            let
                role = result.body
            in
                ( { model | userRole = role }, Cmd.none )

        FillerResp (Ok result) ->
            let
                ugimages = result.body
            in
                ( { model | ugimages_f = Just ugimages }
                , preloadUgImages model.filesrv ugimages
                )

        ValidResp (Ok result) ->
            let
                ugimages = result.body
            in
                ( { model | ugimages_v = Just ugimages }
                , preloadUgImages model.filesrv ugimages
                )

        InvalidResp (Ok result) ->
            let
                ugimages = result.body
            in
                ( { model | ugimages_i = Just ugimages }
                , preloadUgImages model.filesrv ugimages
                )

        ToggleStatementsModal ->
            ( { model | statementsModal = not model.statementsModal }
            , Api.fetchPublicMesAnswers
                { url = model.httpsrv
                , token = ""
                , sub = ""
                }
                |> Task.attempt PublicMesResp
            )

        WindowResize size ->
            ( { model | windowSize = Just size }, Cmd.none )

        FillerResp (Err err) ->
            valuationsErrState model err

        ValidResp (Err err) ->
            valuationsErrState model err

        InvalidResp (Err err) ->
            valuationsErrState model err

        AuthResp (Err err) ->
            httpErrorState model err

        UserResp (Err err) ->
            httpErrorState model err

        GameResp (Err err) ->
            httpErrorState model err

        UsersResp (Err err) ->
            httpErrorState model err

        RegisterUserResp (Err err) ->
            httpErrorState model err

        GroupResp (Err err) ->
            httpErrorState model err

        RoleResp (Err err) ->
            httpErrorState model err

        MesAuthorsResp (Err _) ->
            ( model
            , Cmd.none
            )

        MesAnswersResp (Err _) ->
            ( model
            , Cmd.none
            )

        MesResp (Err _) ->
            ( model
            , Cmd.none
            )

        PublicMesResp (Err _) ->
            ( model
            , Cmd.none
            )

        MesQuerysResp (Err err) ->
            httpErrorState model err

        PutMesResp (Err err) ->
            httpErrorState model err

        ClickedLink _ ->
            (model, Cmd.none)

        ChangedUrl url ->
            onUpdateLocation url model


initFmriStopSignal : Model -> ( Model, Cmd Msg )
initFmriStopSignal model =
    case model.stopsignalGame of
        Nothing ->
            ( model
            , Cmd.none
            )

        Just gameEntity ->
            let
                images_v =
                    model.fmriUserData
                        |> RemoteData.toMaybe
                        |> Maybe.map (\{ ugimages_v } -> Just ugimages_v)
                        |> Maybe.withDefault model.ugimages_v

                images_i =
                    model.fmriUserData
                        |> RemoteData.toMaybe
                        |> Maybe.map (\{ ugimages_i } -> Just ugimages_i)
                        |> Maybe.withDefault model.ugimages_i

                game time seed =
                    Game.Implementations.FmriStopSignal.init
                        { borderDelay = Duration.milliseconds 100
                        , totalDuration = Duration.milliseconds 1000
                        , infoString = """
<h3 class="title">Instructions</h3>
You will see pictures presented in either a dark blue or light gray border. Press the space bar as quickly as you can. BUT only if you see a blue border around the picture. Do not press if you see a gray border. Go as fast as you can, but don't sacrifice accuracy for speed.
<br>
<br>
**Press any key to continue.**
"""
                        , responseImages = getFullImagePathsNew model.filesrv images_v |> Maybe.withDefault []
                        , nonResponseImages = getFullImagePathsNew model.filesrv images_i |> Maybe.withDefault []
                        , seedInt = seed
                        , currentTime = time
                        , blockDuration = Duration.seconds 150
                        , restDuration = Duration.seconds 5
                        , totalBlocks = 4
                        , redCrossDuration = Duration.milliseconds 500
                        , intervalMin = Duration.seconds 3
                        , intervalJitter = Duration.seconds 5
                        }

                gameCmd =
                    Time.now
                        |> Task.map
                            (\time ->
                                let
                                    seed =
                                        42
                                in
                                ( time
                                , seed
                                , game time seed
                                )
                            )
                        |> Task.perform (\( time, seed, ( tempGame, nextSeed ) ) -> StartSession { gameId = gameEntity.id, game = tempGame, time = time, initialSeed = seed, nextSeed = nextSeed })
            in
            ( model
            , gameCmd
            )


initStopSignal : Model -> ( Model, Cmd Msg )
initStopSignal model =
    case model.stopsignalGame of
        Nothing ->
            ( model
            , Cmd.none
            )

        Just gameEntity ->
            let
                ugimages_v =
                    model.ugimages_v

                ugimages_i =
                    model.ugimages_i

                game time seed =
                    Game.Implementations.StopSignal.init
                        { borderDelay = Duration.milliseconds 100
                        , totalDuration = Duration.milliseconds 1000
                        , infoString = """
<h3 class="title">Instructions</h3>
You will see pictures presented in either a dark blue or light gray border. Press the space bar as quickly as you can. BUT only if you see a blue border around the picture. Do not press if you see a gray border. Go as fast as you can, but don't sacrifice accuracy for speed.
<br>
<br>
**Press any key to continue.**
"""
                        , responseImages = getFullImagePathsNew model.filesrv ugimages_v |> Maybe.withDefault []
                        , nonResponseImages = getFullImagePathsNew model.filesrv ugimages_i |> Maybe.withDefault []
                        , seedInt = seed
                        , currentTime = time
                        , redCrossDuration = Duration.milliseconds 500
                        , blockDuration = Duration.minutes 1
                        , totalBlocks = 5
                        , restDuration = Duration.seconds 10
                        , intervalMin = Duration.milliseconds 500
                        , intervalJitter = Duration.milliseconds 0
                        }

                gameCmd =
                    Time.now
                        |> Task.map
                            (\time ->
                                let
                                    seed = Time.posixToMillis time
                                in
                                ( time
                                , seed
                                , game time seed
                                )
                            )
                        |> Task.perform (\( time, seed, ( tempGame, nextSeed ) ) -> StartSession { gameId = gameEntity.id, game = tempGame, time = time, initialSeed = seed, nextSeed = nextSeed })
            in
            ( model
            , gameCmd
            )


initGoNoGo : Model -> ( Model, Cmd Msg )
initGoNoGo model =
    case model.gonogoGame of
        Nothing ->
            ( model
            , Cmd.none
            )

        Just gameEntity ->
            ( model
            , Time.now
                |> Task.map
                    (\time ->
                        let
                            seed = Time.posixToMillis time
                        in
                        ( time
                        , seed
                        , Game.Implementations.GoNoGo.init
                            { totalDuration = Duration.milliseconds 1250
                            , infoString = """
<h3 class="title">Instructions</h3>
<p>You will see pictures either on the left or right side of the screen, surrounded by a solid or dashed border. Press <span class="highlight"><strong>c</strong></span> when the picture is on the left side of the screen or <span class="highlight"><strong>m</strong></span> when the picture is on the right side of the screen. BUT only if you see a <span style="border: 1px solid rgb(0, 0, 0); padding: 2px;">solid border</span> around the picture. Do not press if you see a <span style="border: 1px dashed rgb(0, 0, 0); padding: 2px;">dashed border</span>. Go as fast as you can, but don't sacrifice accuracy for speed.<div>
<br>
<br>
<strong>Press any key or tap here to continue.</strong></div>
</p>
"""
                            , responseImages = getFullImagePathsNew model.filesrv model.ugimages_v |> Maybe.withDefault []
                            , nonResponseImages = getFullImagePathsNew model.filesrv model.ugimages_i |> Maybe.withDefault []
                            , fillerImages = getFullImagePathsNew model.filesrv model.ugimages_f |> Maybe.withDefault []
                            , seedInt = seed
                            , currentTime = time
                            , redCrossDuration = Duration.milliseconds 500
                            , blockDuration = Duration.minutes 1
                            , totalBlocks = 5
                            , restDuration = Duration.seconds 10
                            , intervalMin = Duration.milliseconds 500
                            , intervalJitter = Duration.milliseconds 0
                            }
                        )
                    )
                |> Task.perform (\( time, seed, ( game, nextSeed ) ) -> StartSession { gameId = gameEntity.id, game = game, time = time, initialSeed = seed, nextSeed = nextSeed })
            )


initDotProbe : Model -> ( Model, Cmd Msg )
initDotProbe model =
    case model.dotprobeGame of
        Nothing ->
            ( model
            , Cmd.none
            )

        Just gameEntity ->
            ( model
            , Time.now
                |> Task.map
                    (\time ->
                        let
                            seed = Time.posixToMillis time
                        in
                        ( time
                        , seed
                        , Game.Implementations.DotProbe.init
                            { fixationDuration = Duration.milliseconds 500
                            , imageDuration = Duration.milliseconds 500
                            , infoString = """
<h3 class="title">Instructions</h3>
You will see pictures on the left and right side of the screen, followed by a dot on the left or right side of the screen. Press the <span class="highlight"><strong>c</strong></span> if the dot is on the left side of the screen or <span class="highlight"><strong>m</strong></span> when the dot is on the right side of the screen. Go as fast as you can, but don't sacrifice accuracy for speed.<div>
<br>
<br>
<strong>Press any key or tap here to continue.</strong>
</div>
        """
                            , responseImages = getFullImagePathsNew model.filesrv model.ugimages_v |> Maybe.withDefault []
                            , nonResponseImages = getFullImagePathsNew model.filesrv model.ugimages_i |> Maybe.withDefault []
                            , seedInt = seed
                            , currentTime = time
                            , blockDuration = Duration.minutes 1
                            , totalBlocks = 5
                            , restDuration = Duration.seconds 10
                            , intervalMin = Duration.milliseconds 500
                            , intervalJitter = Duration.milliseconds 0
                            }
                        )
                    )
                |> Task.perform (\( time, seed, ( game, nextSeed ) ) -> StartSession { gameId = gameEntity.id, game = game, time = time, initialSeed = seed, nextSeed = nextSeed })
            )


initVisualSearch : Model -> ( Model, Cmd Msg )
initVisualSearch model =
    case model.visualsearchGame of
        Nothing ->
            ( model
            , Cmd.none
            )

        Just gameEntity ->
            ( model
            , Time.now
                |> Task.map
                    (\time ->
                        let
                            seed = Time.posixToMillis time
                        in
                        ( time
                        , seed
                        , Game.Implementations.VisualSearch.init
                            { fixationDuration = Duration.milliseconds 500
                            , imageDuration = Duration.milliseconds 3000
                            , zoomDuration = Duration.milliseconds 1000
                            , infoString = """
<h3 class="title">Instructions</h3>
You will see a grid of images. Select the target image as quickly as you can.
<br>
<br>
<strong>Press any key or tap here to continue.</strong>
</div>
"""
                            , responseImages = getFullImagePathsNew model.filesrv model.ugimages_v |> Maybe.withDefault []
                            , nonResponseImages = getFullImagePathsNew model.filesrv model.ugimages_i |> Maybe.withDefault []
                            , seedInt = seed
                            , currentTime = time
                            , blockDuration = Duration.minutes 1
                            , totalBlocks = 5
                            , restDuration = Duration.seconds 10
                            , intervalMin = Duration.milliseconds 500
                            , intervalJitter = Duration.milliseconds 0
                            }
                        )
                    )
                |> Task.perform (\( time, seed, ( game, nextSeed ) ) -> StartSession { gameId = gameEntity.id, game = game, time = time, initialSeed = seed, nextSeed = nextSeed })
            )


getFullImagePaths : String -> Maybe (List Entity.Ugimage) -> Maybe (List String)
getFullImagePaths prefix =
    Maybe.map (List.filterMap .gimage >> List.map (.path >> (++) (prefix ++ "/repo/")))


preloadUgImages : String -> List Entity.Ugimage -> Cmd Msg
preloadUgImages prefix images =
    getFullImagePaths prefix (Just images)
        |> Maybe.withDefault []
        |> Port.preload


valuationsErrState : Model -> ValuationsError -> ( Model, Cmd msg )
valuationsErrState model err =
    ( { model
        | loading = Nothing
        , glitching = Just (valuationsError err)
        , httpErr = valuationsError err
      }
    , Cmd.none
    )


httpErrorState : Model -> Http.Detailed.Error String -> ( Model, Cmd msg )
httpErrorState model err =
    ( { model
        | loading = Nothing
        , glitching = Just (Helpers.httpHumanError err)
        , httpErr = Helpers.httpHumanError err
      }
    , Cmd.none
    )


valuationsError : ValuationsError -> String
valuationsError err =
    case err of
        ReqFail httpErr ->
            Helpers.httpHumanError httpErr

        MissingValuations ->
            "You are missing customized game images! Are your image valuations uploaded?"



-- Instruction Views


highlight : String -> Html Msg
highlight l =
    span [ class "highlight" ] [ strong [] [ text l ] ]


solid : String -> Html Msg
solid t =
    span [ style "border" "solid 1px #000", style "padding" "2px" ] [ text t ]


dashed : String -> Html Msg
dashed t =
    span [ style "border" "dashed 1px #000", style "padding" "2px" ] [ text t ]


base : List (Html Msg) -> Html Msg
base kids =
    div [ class "columns" ]
        [ div [ class "column is-half is-offset-one-quarter" ]
            [ div [ class "box" ]
                kids
            ]
        ]


title : Html Msg
title =
    h3 [ class "title" ] [ text "Instructions" ]


pressAnyKey : Html Msg
pressAnyKey =
    div []
        [ br [] []
        , br [] []
        , strong [] [ text "Press any key to continue." ]
        ]


dpInstructions : Html Msg
dpInstructions =
    base
        [ title
        , text "You will see pictures on the left and right side of the screen, followed by a dot on the left or right side of the screen. Press the "
        , highlight "c"
        , text " if the dot is on the left side of the screen or "
        , highlight "m"
        , text " when the dot is on the right side of the screen. Go as fast as you can, but don't sacrifice accuracy for speed."
        , pressAnyKey
        ]


vsInstructions : Html Msg
vsInstructions =
    base
        [ title
        , text "You will see a grid of images. Select the target image as quickly as you can."
        , pressAnyKey
        ]


rsInstructions : Html Msg
rsInstructions =
    base
        [ title
        , text "You will see pictures on the screen. Some of the pictures will be followed by a tone (a beep). Please press the space bar as quickly as you can. BUT only if you hear a beep after the picture. Do not press if you do not hear a beep."
        , pressAnyKey
        ]


ssInstructions : Html Msg
ssInstructions =
    base
        [ title
        , text "You will see pictures presented in either a dark blue or light gray border. Press the space bar as quickly as you can. BUT only if you see a blue border around the picture. Do not press if you see a gray border. Go as fast as you can, but don't sacrifice accuracy for speed."
        , pressAnyKey
        ]


gngInstructions : Html Msg
gngInstructions =
    base
        [ title
        , p []
            [ text "You will see pictures either on the left or right side of the screen, surrounded by a solid or dashed border. Press "
            , highlight "c"
            , text " when the picture is on the left side of the screen or "
            , highlight "m"
            , text " when the picture is on the right side of the screen. BUT only if you see a "
            , solid "solid border"
            , text " around the picture. Do not press if you see a "
            , dashed "dashed border"
            , text ". Go as fast as you can, but don't sacrifice accuracy for speed."
            , pressAnyKey
            ]
        ]


missing : Entity.UserRecord -> Bool
missing ur =
    List.member ""
        [ ur.username
        , ur.groupId
        , ur.password
        ]


startSession : { gameId : String, game : Game.Game Msg, time : Time.Posix, initialSeed : Int, nextSeed : Random.Seed } -> Model -> ( Model, Cmd Msg )
startSession { gameId, game, time, initialSeed, nextSeed } model =
    case model.visitor of
        Anon ->
            ( model
            , Cmd.none
            )

        LoggedIn jwt ->
            ( { model | gameState = Game.Loading game RemoteData.Loading }
            , Api.startSession
                { token = model.jwtencoded
                , userId = model.fmriUserData |> RemoteData.toMaybe |> Maybe.map (\{ user } -> user.id) |> Maybe.withDefault jwt.sub
                , gameId = gameId
                , start = time
                , initialSeed = initialSeed
                , httpsrv = model.httpsrv
                , jitter = model.fmriUserData |> RemoteData.toMaybe |> Maybe.map (always True) |> Maybe.withDefault False
                }
                |> Task.perform (StartSessionResp nextSeed game)
            )


playGame : Game.Game Msg -> Game.Session -> Random.Seed -> Model -> ( Model, Cmd Msg )
playGame game session nextSeed model =
    handleInput Game.Initialize
        { model
            | gameState = Game.Playing { game = game, session = session, nextSeed = nextSeed }
            , glitching = Nothing
        }



-- GAME ENGINE


handleInput : Game.Input -> Model -> ( Model, Cmd Msg )
handleInput input model =
    case model.gameState of
        Game.NotPlaying ->
            ( model, Cmd.none )

        Game.Loading _ _ ->
            ( model, Cmd.none )

        Game.Playing { game, session, nextSeed } ->
            case Game.Card.step input game of
                ( Game.Card.Complete state, cmd ) ->
                    let
                        updatedSession =
                            { session | end = Just state.currTime }

                        updatedModel =
                            { model | gameState = Game.Saving state updatedSession RemoteData.Loading }
                    in
                    ( updatedModel
                    , Cmd.batch
                        [ cmd
                        , saveGameDataCmd state updatedSession updatedModel
                        ]
                    )

                ( Game.Card.Continue _ newGame, cmd ) ->
                    ( { model | gameState = Game.Playing { game = newGame, session = session, nextSeed = nextSeed } }, cmd )

                ( Game.Card.Rest _ newGame, cmd ) ->
                    ( { model | gameState = Game.Playing { game = newGame, session = session, nextSeed = nextSeed } }, cmd )

                ( Game.Card.Restart state args, cmd ) ->
                    ( { model | gameState = Game.restart args state model.gameState }, cmd )

        Game.Saving _ _ _ ->
            ( model, Cmd.none )

        Game.Saved _ _ ->
            ( model, Cmd.none )


getFullImagePathsNew : String -> Maybe (List Entity.Ugimage) -> Maybe (List Game.Image)
getFullImagePathsNew prefix maybeUgimages =
    let
        toImages ugimages =
            ugimages
                |> List.filterMap toImage

        toImage ugimage =
            case ugimage.gimage of
                Nothing ->
                    Nothing

                Just gimage ->
                    Just
                        { url = prefix ++ "/repo/" ++ gimage.path
                        , id = ugimage.id
                        }
    in
    Maybe.map toImages maybeUgimages


presses : String -> Model -> ( Model, Cmd Msg )
presses keyCode model =
    let
        ( newModel1, cmd1 ) =
            if keyCode == " " {- space -}
                || keyCode == "0"
                || keyCode == "1"
                || keyCode == "2"
                || keyCode == "3"
                || keyCode == "4"
                || keyCode == "5"
                || keyCode == "6"
                || keyCode == "7"
                || keyCode == "8"
                || keyCode == "9" {- numeric -} then
                    handleInput Game.Indication model

                else
                    ( model, Cmd.none )

        ( newModel2, cmd2 ) =
            case keyCode of
                "c" ->
                    {- c -}
                    handleInput (Game.Direction Game.Left) newModel1

                "m" ->
                    {- m -}
                    handleInput (Game.Direction Game.Right) newModel1

                _ ->
                    ( newModel1, Cmd.none )

        cmd3 =
            case ( keyCode, newModel2.gameState, RemoteData.toMaybe newModel2.fmriUserData ) of
                ( "'" {- ' -}, Game.NotPlaying, Just { user } ) ->
                    Task.perform InitFmriStopSignal (Task.succeed user)

                _ ->
                    Cmd.none
    in
    ( newModel2, Cmd.batch [ cmd1, cmd2, cmd3 ] )


handleSelectInput : Int -> Model -> ( Model, Cmd Msg )
handleSelectInput n model =
    handleInput (Game.Select n) model


handleDirectionInput : Game.Direction -> Model -> ( Model, Cmd Msg )
handleDirectionInput direction model =
    handleInput (Game.Direction direction) model


handleIndicationInput : Model -> ( Model, Cmd Msg )
handleIndicationInput model =
    handleInput Game.Indication model


handleTimeUpdate : Time.Posix -> Model -> ( Model, Cmd Msg )
handleTimeUpdate t model =
    handleInput (Game.Tick t) model


startSessionResp : Random.Seed -> Game.Game Msg -> (RemoteData.RemoteData (Http.Detailed.Error String) (Http.Detailed.Success Game.Session)) -> Model -> ( Model, Cmd Msg )
startSessionResp nextSeed game remoteData model =
    let
        updatedModel =
            { model | gameState = Game.Loading game remoteData }
    in
    case remoteData of
        RemoteData.Success session ->
            playGame game session.body nextSeed updatedModel

        RemoteData.Failure err ->
            ( { updatedModel | glitching = Just <| Helpers.httpHumanError err }
            , Cmd.none
            )

        RemoteData.Loading ->
            ( updatedModel
            , Cmd.none
            )

        RemoteData.NotAsked ->
            ( updatedModel
            , Cmd.none
            )


gameDataSaved : Game.State -> Game.Session -> (RemoteData.RemoteData (Http.Detailed.Error String) ( (Http.Detailed.Success Game.Session), (Http.Detailed.Success (List Game.Cycle) ))) -> Model -> ( Model, Cmd Msg )
gameDataSaved state session remoteData model =
    let
        updatedModel =
            { model | gameState = Game.Saving state session remoteData }
    in
    case remoteData of
        RemoteData.Success ( tempSession, cycles ) ->
            ( { model
                | gameState = Game.Saved state { session = tempSession.body, cycles = cycles.body }
                , fmriUserData = RemoteData.NotAsked
              }
            , Cmd.none
            )

        RemoteData.Failure err ->
            ( { updatedModel | glitching = Just <| Helpers.httpHumanError err }
            , Cmd.none
            )

        RemoteData.Loading ->
            ( updatedModel
            , Cmd.none
            )

        RemoteData.NotAsked ->
            ( updatedModel
            , Cmd.none
            )


saveGameData : Game.State -> Game.Session -> Model -> ( Model, Cmd Msg )
saveGameData state session model =
    let
        updatedModel =
            { model | gameState = Game.Saving state session RemoteData.Loading }
    in
    ( updatedModel
    , saveGameDataCmd state session updatedModel
    )


saveGameDataCmd : Game.State -> Game.Session -> Model -> Cmd Msg
saveGameDataCmd state session model =
    let
        endSessionTask =
            Api.endSession
                { session = session
                , token = model.jwtencoded
                , httpsrv = model.httpsrv
                }

        postCyclesTask =
            Api.postCycles
                { session = session
                , cycles = cycles
                , token = model.jwtencoded
                , httpsrv = model.httpsrv
                }

        cycles =
            Game.Cycle.generate session.id state.log
    in
    Task.map2 (\a b -> ( a, b )) endSessionTask postCyclesTask
        |> Task.map (\( a, b ) -> RemoteData.map2 (\c d -> ( c, d )) a b)
        |> Task.perform (GameDataSaved state session)


fix_email : Entity.UserRecord -> Entity.UserRecord
fix_email ur =
    if ur.email == "" then
        { ur | email = ur.username ++ "@example.com" }

    else
        ur


fmriImagesResp : RemoteData.RemoteData ValuationsError Model.FmriUserData -> Model -> ( Model, Cmd Msg )
fmriImagesResp resp model =
    case resp of
        RemoteData.Failure error ->
            valuationsErrState model error

        RemoteData.Success fmriUserData ->
            ( { model | fmriUserData = resp }
            , Cmd.batch
                [ preloadUgImages model.filesrv (fmriUserData.ugimages_v ++ fmriUserData.ugimages_i ++ fmriUserData.ugimages_f)
                , pushUrl model.key "stopsignal"
                ]
            )

        RemoteData.Loading ->
            ( model, Cmd.none )

        RemoteData.NotAsked ->
            ( model, Cmd.none )


onUpdateLocation : Url.Url -> Model -> ( Model, Cmd Msg )
onUpdateLocation location model =
    let
        model_ =
            { model
                | activeRoute =
                    model.visitor
                        |> Helpers.getJwt
                        |> Maybe.map (Helpers.checkAccess <| R.parseLocation location)
                        |> Maybe.withDefault R.LoginRoute
                , isMenuActive = False
                , gameState = Game.NotPlaying
            }
    in
    refetchDataOnChange model_


refetchDataOnChange : Model -> ( Model, Cmd Msg )
refetchDataOnChange model =
    case model.activeRoute of
        R.AdminRoute ->
            ( model, Task.attempt UsersResp (Api.fetchUsers_ model.httpsrv model.jwtencoded) )

        R.FmriRoute _ ->
            Api.fetchFmriUserData model

        _ ->
            ( model, Cmd.none )
