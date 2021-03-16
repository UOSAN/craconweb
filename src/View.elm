module View exposing (view)

import Browser exposing (Document)
import Entity
import Game
import Game.View as Game
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import List.Extra
import Model exposing (..)
import RemoteData
import Routing as R
import Ui.Admin as Admin
import Ui.Parts as Parts


bigLogo : String -> Html Msg
bigLogo filesrv =
    p [ style "text-align" "center" ]
        [ img
            [ class "logo is-vcentered"
            , src (filesrv ++ "/repo/logo.svg")
            , style "max-width" "300px"
            ]
            []
        ]


statementsModalButtonClass : Bool -> String
statementsModalButtonClass isOn =
    if isOn then
        "button is-fullwidth is-info"
    else
        "button is-fullwidth is-info is-outlined"


statementsModal : Model -> Html Msg
statementsModal model =
    if model.statementsModal then
        Parts.modalCard
            ToggleStatementsModal
            "Statements from participants"
            (statements 2 model.statements)
    else
        text ""


loginPage : Model -> Html Msg
loginPage model =
    section []
        [ statementsModal model
        , div [ class "container scale-in-center" ]
            [ div [ class "columns is-desktop" ]
                [ div [ class "column is-half is-offset-one-quarter" ]
                    [ bigLogo model.filesrv
                    , loginPageBoxForm model
                    , Parts.notification model.glitching "is-warning"
                    , h4 [ class "is-4 subtitle is-centered", style "text-align" "center" ]
                        [ strong [] [ text "Not already a member of the CraveControl study?" ]
                        , br [] []
                        , text "Click "
                        , a [ href "https://ori.qualtrics.com/jfe/form/SV_0wxvIDQwmJLlL2B", target "_blank" ] [ text "here" ]
                        , text " to see if you're eligible to participate!"
                        , br [] []
                        ]
                    , div [ class "is-centered", style "text-align" "center" ]
                        [ button
                            [ class <| statementsModalButtonClass model.statementsModal
                            , onClick ToggleStatementsModal
                            ]
                            [ text "Read Statements From Participants" ]
                        ]
                    ]
                ]
            ]
        ]


loginPageButtonClass : Maybe String -> String
loginPageButtonClass loading =
    case loading of
        Just _ ->
            "button is-fullwidth is-success is-loading"

        Nothing ->
            "button is-fullwidth is-success"


loginPageBoxForm : Model -> Html Msg
loginPageBoxForm model =
    div [ class "box", marginS ]
        [ Html.form
            [ onSubmit TryLogin ]
            [ p [ class "control" ]
                [ input
                    [ class "input"
                    , placeholder "Username"
                    , type_ "text"
                    , onInput UpdateEmail
                    ]
                    []
                ]
            , br [] []
            , p [ class "control" ]
                [ input
                    [ class "input"
                    , placeholder "Password"
                    , type_ "password"
                    , onInput UpdatePassword
                    ]
                    []
                ]
            , hr []
                []
            , p [ class "control" ]
                [ button
                    [ class <| loginPageButtonClass model.loading
                    , type_ "submit"
                    ]
                    [ text "Let's Go!" ]
                ]
            ]
        ]


navBar : Model -> Html Msg
navBar model =
    case model.activeRoute of
        R.FmriRoute _ ->
            div
                [ style "min-height" "15vh"
                , style "cursor" "none"
                ]
                []

        _ ->
            nav
                [ class "nav has-shadow" ]
                [ div [ class "container" ]
                    [ div [ class "nav-left" ]
                        [ tinyLogo model.filesrv
                        ]
                    , navToggler model.isMenuActive
                    , navRight model.isMenuActive model.activeRoute model.visitor
                    ]
                ]


isPowerful : Visitor -> Bool
isPowerful visitor =
    case visitor of
        LoggedIn jwt ->
            List.map .name jwt.roles
                |> isAdminOrStaff

        _ ->
            False


isAdminOrStaff : List String -> Bool
isAdminOrStaff roles =
    List.member "admin" roles || List.member "staff" roles


navToggler : Bool -> Html Msg
navToggler activeMenu =
    span
        [ class <| "nav-toggle" ++ isActive activeMenu, onClick MainMenuToggle ]
        [ span [] []
        , span [] []
        , span [] []
        ]


adminLink : Visitor -> Html Msg
adminLink visitor =
    if isPowerful visitor then
        navLink "Admin" R.adminPath False

    else
        div [] []


navRight : Bool -> R.Route -> Visitor -> Html Msg
navRight activeMenu activeRoute visitor =
    div
        [ id "nav-menu", class <| "nav-right nav-menu" ++ isActive activeMenu ]
        [ navLink "Games" R.homePath (R.HomeRoute == activeRoute)
        , navLink "Badges" R.badgesPath (R.BadgesRoute == activeRoute)
        , navLink "Instructions" R.instructionsPath (R.InstructionsRoute == activeRoute)
        , navLink "Statements" R.statementsPath (R.StatementsRoute == activeRoute)
        , adminLink visitor
        , a [ class "nav-item is-tab", onClick Logout ] [ text "Logout" ]
        ]


navLink : String -> String -> Bool -> Html Msg
navLink text_ path active =
    let
        class_ =
            if active then
                "nav-item is-tab is-active"

            else
                "nav-item is-tab"
    in
    a (( class class_ ) :: Parts.linkAttrs path)
        [ text text_ ]


tinyLogo : String -> Html Msg
tinyLogo filesrv =
    a
        [ class "nav-item"
        , href <| R.homePath
        , R.onLinkClick <| UpdateLocation R.homePath
        ]
        [ img [ src (filesrv ++ "/repo/logo.png") ] [] ]


isActive : Bool -> String
isActive active =
    if active then
        " is-active"

    else
        ""


basicPage : Model -> List (Html Msg) -> Html Msg
basicPage model children =
    section []
        [ mesQueryModal model.mesQuery model.request
        , navBar model
        , section
            [ class "section" ]
            children
        , Parts.notification model.glitching "is-danger"
        ]


homePage : Model -> Html Msg
homePage model =
    div []
        [ mesQueryModal model.mesQuery model.request
        , navBar model
        , homePageBody model
        , Parts.notification model.glitching "is-danger"
        , loading_screen (not model.domLoaded)
        ]


loading_screen : Bool -> Html msg
loading_screen is_loading =
    if is_loading then
        Parts.modal [ section [ class "modal-card-body" ] [ h1 [ class "title" ] [ text "Loading games...", i [ class "fa fa-loading fa-spin" ] [] ] ] ]
    else
        text ""


homePageBody : Model -> Html Msg
homePageBody model =
    section
        [ class "hero is-primary" ]
        [ div
            [ class "hero-body" ]
            [ div
                [ class "container" ]
                [ h1 [ class "title is-1" ] [ welcome model.user ]
                , homePageGrid model
                ]
            ]
        ]


welcome : Maybe Entity.User -> Html msg
welcome user =
    case user of
        Just u ->
            text <| "Welcome, " ++ u.firstName

        Nothing ->
            text ""


homePageGrid : Model -> Html Msg
homePageGrid model =
    div [ class "columns" ]
        (homePageGameCards model)


homePageGameCards : Model -> List (Html Msg)
homePageGameCards model =
    let
        toCard gameArg =
            case gameArg of
                Just g ->
                    div [ class "column" ]
                        [ homePageGameCard g.slug (model.filesrv ++ "/repo/" ++ g.icon) g.name g.dscript ]

                Nothing ->
                    div [] []
    in
    List.map toCard
        [ model.gonogoGame
        , model.dotprobeGame
        , model.stopsignalGame
        , model.visualsearchGame
        ]


homePageGameCard : String -> String -> String -> String -> Html Msg
homePageGameCard gameSlug src_ title about =
    div
        [ class "card", style "border-radius" "1em" ]
        [ div
            [ class "card-image" ]
            [ figure
                [ class "image is-4by3" ]
                [ a (Parts.linkAttrs gameSlug)
                    [ img
                        [ src src_
                        , alt title
                        ]
                        []
                    ]
                ]
            ]
        , div
            [ class "card-content" ]
            [ div
                [ class "media" ]
                [ div
                    [ class "media-content" ]
                    [ strong
                        []
                        [ text title
                        ]
                    ]
                ]
            , div
                [ class "content" ]
                [ text about
                ]
            ]
        ]


mesQueryModal : Maybe String -> Maybe String -> Html Msg
mesQueryModal q feedback =
    case q of
        Nothing ->
            text ""

        Just question ->
            Parts.modal
                [ div
                    [ class "field" ]
                    [ label
                        [ class "label white title is-3" ]
                        [ text question ]
                    , p
                        [ class "control" ]
                        [ textarea
                            [ class "textarea"
                            , placeholder "Please answer the above question."
                            , onInput UpdateMesAnswer
                            ]
                            []
                        ]
                    , p [ class "questionFeedback" ] [ Maybe.withDefault "" feedback |> text ]
                    , button
                        [ class "button is-primary is-large"
                        , onClick TrySubmitMesAnswer
                        ]
                        [ text "Share" ]
                    ]
                ]


accessDeniedPage : Model -> Html Msg
accessDeniedPage model =
    basicPage model
        [ div
            [ class "container" ]
            [ h1 [ class "title is-1" ] [ text "Access Restricted." ]
            , h3 [] [ text "Maybe you are in the wrong place?" ]
            ]
        ]


notFoundPage : Model -> Html Msg
notFoundPage model =
    basicPage model
        [ div
            [ class "container" ]
            [ Parts.middleBlock
                [ h1 [ class "title is-1" ] [ text "Poem 404" ]
                , h5 [ class "subtitle is-5" ] [ text "Page Not Found" ]
                , p []
                    [ text "I shall be telling this with a sigh"
                    , br [] []
                    , text "Somewhere ages and ages hence:"
                    , br [] []
                    , text "Two roads diverged in a wood, and I-"
                    , br [] []
                    , text "I took the one less traveled by,"
                    , br [] []
                    , text "And that has made all the difference."
                    , br [] []
                    ]
                , em [] [ text "- Robert Frost" ]
                , br [] []
                ]
            ]
        ]


badgesPage : Model -> Html Msg
badgesPage model =
    basicPage model
        [ div
            [ class "container swing-in-top-fwd" ]
            [ h1 [ class "title is-1" ] [ text "Badges" ]
            , div []
                (rules model.badgeRules model.badgesEarned)
            ]
        ]


tryUnlock : List a -> { dscript : String, id : a, name : String } -> { name : String, dscript : String, unlocked : Bool, fg : String, bg : String }
tryUnlock ids b =
    let
        b_ =
            { name = b.name, dscript = b.dscript, unlocked = True, fg = "#000", bg = "#eee" }
    in
    if List.member b.id ids then
        { b_ | unlocked = True, bg = "#F2E86B" }
    else
        { b_ | unlocked = False }


rules :
    RemoteData.RemoteData e (List { dscript : String, id : a, name : String })
    -> RemoteData.RemoteData e1 (List a)
    -> List (Html Msg)
rules badges earned =
    case badges of
        RemoteData.Success badges_ ->
            case earned of
                RemoteData.Success earned_ ->
                    List.map (tryUnlock earned_) badges_
                        |> List.map badgeBox
                        |> Parts.grid 3

                _ ->
                    [ text "" ]

        _ ->
            [ text "" ]


badgeBox :
    { bg : String
    , dscript : String
    , fg : String
    , name : String
    , unlocked : Bool
    }
    -> Html msg
badgeBox b =
    let
        ( wobble, icon ) =
            if b.unlocked then
                ( "wobble-hor-bottom", i [ class "fa fa-unlock" ] [] )

            else
                ( "", i [ class "fa fa-lock" ] [] )
    in
    div [ class <| "box " ++ wobble, style "background-color" b.bg, style "color" b.fg ]
        [ h1 [ class "title is-1", style "color" b.fg ] [ text <| b.name ++ " ", icon ]
        , p [] [ text b.dscript ]
        ]


settingsPage : Model -> Html Msg
settingsPage model =
    basicPage model
        [ div
            [ class "container" ]
            [ h1 [ class "title is-1" ] [ text "Settings" ]
            , h3 [] [ text "This feature is coming soon!" ]
            ]
        ]


easeIOTransform : Float -> Float -> Float -> Float
easeIOTransform curr base mod =
    let
        m =
            if mod >= 1.0 then
                2.125

            else if mod <= 0.0 then
                1.125

            else
                mod + 1.125
    in
    (curr ^ m) / ((curr ^ m) + ((base - curr) ^ m))


easeInTransform : Float -> Float -> Float -> Float
easeInTransform curr base mod =
    let
        m =
            if mod > 1.0 then
                1.0

            else if mod < 0.0 then
                0.0

            else
                mod
    in
    (curr - (base - curr) * (m * (curr / base))) / base


visSearchScale : WindowSize -> Float
visSearchScale size =
    let
        baseRatio =
            720.0 / 955.0

        currRatio =
            toFloat size.height / toFloat size.width
    in
    if baseRatio > currRatio then
        if size.width < 955 then
            easeInTransform currRatio baseRatio 0.5

        else if size.width < 1384 then
            easeInTransform currRatio baseRatio 0.25

        else
            easeIOTransform currRatio baseRatio 0.5

    else
        1.0


scaleStyleFromFloat : Float -> List (Attribute msg)
scaleStyleFromFloat scale =
    [ style "transform" ("scale(" ++ String.fromFloat scale ++ ")")
    , style "transform-origin" "50% 0"
    ]


scaleStyle : R.Route -> Maybe WindowSize -> List (Attribute msg)
scaleStyle route size =
    case route of
        R.GameRouteVs ->
            case size of
                Just s ->
                    scaleStyleFromFloat <| visSearchScale s

                _ ->
                    []

        _ ->
            []


game : Model -> String -> Msg -> Html Msg
game model title initMsg =
    let
        title_ =
            case model.gameState of
                Game.NotPlaying ->
                    h1 [ class "title is-1" ] [ text title ]

                Game.Loading _ _ ->
                    h1 [ class "title is-1" ] [ text title ]

                _ ->
                    text ""
    in
    basicPage model
        [ div
            ((class "container") :: (scaleStyle model.activeRoute model.windowSize))
            [ title_
            , case model.activeRoute of
                R.GameRouteSs ->
                    Game.view
                        { gameState = model.gameState
                        , initMsg = initMsg
                        , gameSlug = model.stopsignalGame |> Maybe.map .slug |> Maybe.withDefault "stopsignal"
                        , fmriUser = Nothing
                        , restMessages = [ "Great job! We’re going to take a quick 10 second break before we move on to the next section." ]
                        }

                R.GameRouteGn ->
                    Game.view
                        { gameState = model.gameState
                        , initMsg = initMsg
                        , gameSlug = model.gonogoGame |> Maybe.map .slug |> Maybe.withDefault "gonogo"
                        , fmriUser = Nothing
                        , restMessages = [ "Great job! We’re going to take a quick 10 second break before we move on to the next section." ]
                        }

                R.GameRouteDp ->
                    Game.view
                        { gameState = model.gameState
                        , initMsg = initMsg
                        , gameSlug = model.dotprobeGame |> Maybe.map .slug |> Maybe.withDefault "dotprobe"
                        , fmriUser = Nothing
                        , restMessages = [ "Great job! We’re going to take a quick 10 second break before we move on to the next section." ]
                        }

                R.GameRouteVs ->
                    Game.view
                        { gameState = model.gameState
                        , initMsg = initMsg
                        , gameSlug = model.visualsearchGame |> Maybe.map .slug |> Maybe.withDefault "visualsearch"
                        , fmriUser = Nothing
                        , restMessages = [ "Great job! We’re going to take a quick 10 second break before we move on to the next section." ]
                        }

                R.FmriRoute _ ->
                    Game.view
                        { gameState = model.gameState
                        , initMsg = initMsg
                        , gameSlug = model.stopsignalGame |> Maybe.map .slug |> Maybe.withDefault "stopsignal"
                        , fmriUser =
                            model.fmriUserData
                                |> RemoteData.toMaybe
                                |> Maybe.map (\fmriUserData -> fmriUserData.user)
                        , restMessages = []
                        }

                _ ->
                    text "Invalid Game"
            ]
        , loading_screen (RemoteData.isLoading model.fmriUserData)
        ]


visualSearchGame : Model -> Html Msg
visualSearchGame model =
    game model "Visual Search" InitVisualSearch


dotProbeGame : Model -> Html Msg
dotProbeGame model =
    game model "Dot Probe" InitDotProbe


goNoGoGame : Model -> Html Msg
goNoGoGame model =
    game model "Go/No Go" InitGoNoGo


stopSignalGame : Model -> Html Msg
stopSignalGame model =
    case RemoteData.toMaybe model.fmriUserData of
        Nothing ->
            game model "Stop Signal" InitStopSignal

        Just { user } ->
            game model "Stop Signal" (InitFmriStopSignal user)


instructionsPage : Model -> Html Msg
instructionsPage model =
    basicPage model
        [ div
            [ class "container" ]
            [ h1 [ class "title is-1" ]
                [ text "Instructions" ]
            , hr [] []
            , div
                [ class "columns" ]
                [ instBlock "Go/no-go" """You will see pictures either on
                    the left or right side of the screen, surrounded by a solid
                    or dashed border. Press 'c' when the picture is on the left
                    side of the screen or 'm' when the picture is on the right
                    side of the screen. BUT only if you see a solid bar around
                    the picture. Do not press if you see a dashed border. Go as
                    fast as you can, but don't sacrifice accuracy for speed."""
                , instBlock "Dot probe" """You will see pictures on the
                    left and right side of the screen, followed by a dot on the
                    left or right side of the screen. Press the "c" if the dot is
                    on the left side of the screen or "m" when the dot is on the
                    right side of the screen. Go as fast as you can, but don't
                    sacrifice accuracy for speed."""
                , instBlock "Stop Signal" """You will see pictures presented
                     in either a dark blue or light gray border. Press the space
                      bar if you see a blue border around the picture.
                    Do not press if you see a gray border.
                        Go as fast as you can, but don't sacrifice accuracy for speed."""
                , instBlock "Visual search" """You will see a grid of images.
                    Select the target image as quickly as you can. Don't sacrifice
                    accuracy for speed."""
                ]
            ]
        ]


marginS : Attribute msg
marginS =
    style "margin" ".7em"


instBlock : String -> String -> Html Msg
instBlock title content =
    div [ class "column" ]
        [ p [ class "title" ] [ text title ]
        , p [] [ text content ]
        ]


statementsPage : Model -> Html Msg
statementsPage model =
    basicPage model
        (( h1 [ class "title has-text-centered" ] [ text "Statements" ] )
            :: statements 4 model.statements
        )


statements : Int -> Maybe (List MesAnswer) -> List (Html Msg)
statements groupsOfNum mesAnswers =
    case mesAnswers of
        Nothing ->
            [ Parts.middleBlock
                [ p [] [ text """Coming soon! You will be able to see personal
                            statements from other members of the study about their journey to better
                            health and some of the choices they made that helped them get there!""" ]
                ]
            ]

        Just tempMesAnswers ->
            List.map statement tempMesAnswers
                |> List.Extra.greedyGroupsOf groupsOfNum
                |> List.map (div [ class "columns" ])


statement : MesAnswer -> Html Msg
statement mes =
    div [ class "column" ]
        [ blockquote []
            [ i [ class "fa fa-quote-left" ] []
            , p [] [ text mes.essay ]
            , i [ class "fa fa-quote-right" ] []
            , br [] []
            , p [] [ text <| "- " ++ mes.displayName ]
            ]
        ]



{-

   ADMIN VIEWS

-}


view : Model -> Document Msg
view model =
    let
        page =
            case model.activeRoute of
                R.AdminRoute ->
                    adminView model

                R.StatementsRoute ->
                    statementsPage model

                R.RegisterRoute ->
                    Admin.registerPage model

                R.LoginRoute ->
                    loginPage model

                R.HomeRoute ->
                    homePage model

                R.AccessDeniedRoute ->
                    accessDeniedPage model

                R.NotFoundRoute ->
                    notFoundPage model

                R.BadgesRoute ->
                    badgesPage model

                R.SettingsRoute ->
                    settingsPage model

                R.InstructionsRoute ->
                    instructionsPage model

                R.GameRouteVs ->
                    visualSearchGame model

                R.GameRouteDp ->
                    dotProbeGame model

                R.GameRouteGn ->
                    goNoGoGame model

                R.GameRouteSs ->
                    stopSignalGame model

                R.EditUserRoute userid ->
                    Admin.editUserPage model userid

                R.MesRoute ->
                    Admin.mesPage model

                R.FmriRoute _ ->
                    stopSignalGame model
    in
    { title = ""
    , body = [ div [] [ page ] ]
    }


adminView : Model -> Html Msg
adminView model =
    case model.gameState of
        Game.NotPlaying ->
            Admin.adminPage model

        _ ->
            stopSignalGame model
