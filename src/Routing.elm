module Routing exposing (..)

import Html exposing (Attribute)
import Html.Events exposing (onWithOptions)
import Json.Decode as Decode
import Navigation
import UrlParser exposing (..)


powerPaths : List String
powerPaths =
    [ adminPath, registerPath, mesPath ]


parseLocation : Navigation.Location -> Route
parseLocation location =
    case parsePath matchers location of
        Just route ->
            route

        Nothing ->
            NotFoundRoute


onLinkClick : msg -> Attribute msg
onLinkClick message =
    let
        options =
            { stopPropagation = False
            , preventDefault = True
            }
    in
    onWithOptions "click" options (Decode.succeed message)


{-| Represents where I am in the application
-}
type Route
    = AccessDeniedRoute
    | NotFoundRoute
    | LoginRoute
    | HomeRoute
    | GameRouteVs
    | GameRouteDp
    | GameRouteGn
    | GameRouteSs
    | BadgesRoute
    | SettingsRoute
    | InstructionsRoute
    | StatementsRoute
    | AdminRoute
    | RegisterRoute
    | EditUserRoute String
    | MesRoute
    | FmriRoute String


homePath : String
homePath =
    "/"


loginPath : String
loginPath =
    "/login"


adminPath : String
adminPath =
    "/admin"


registerPath : String
registerPath =
    "/register"


logoutPath : String
logoutPath =
    "/logout"


badgesPath : String
badgesPath =
    "/badges"


statementsPath : String
statementsPath =
    "/statements"


settingsPath : String
settingsPath =
    "/settings"


instructionsPath : String
instructionsPath =
    "/instructions"


mesPath : String
mesPath =
    "/mes"


editPath : String
editPath =
    "/edit/"


{-| Define how to match urls
-}
matchers : Parser (Route -> a) a
matchers =
    oneOf
        [ map HomeRoute top
        , map LoginRoute (s "login")
        , map GameRouteVs (s "visualsearch")
        , map GameRouteDp (s "dotprobe")
        , map GameRouteGn (s "gonogo")
        , map GameRouteSs (s "stopsignal")
        , map BadgesRoute (s "badges")
        , map SettingsRoute (s "settings")
        , map StatementsRoute (s "statements")
        , map InstructionsRoute (s "instructions")
        , map AdminRoute (s "admin")
        , map RegisterRoute (s "register")
        , map EditUserRoute (s "edit" </> string)
        , map MesRoute (s "mes")
        , map FmriRoute (s "fmri" </> string)
        ]
