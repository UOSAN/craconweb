module Helpers exposing
    ( httpHumanError
    , checkAccess
    , getJwt
    , isAdmin
    , isStaff
    , keyDecoder
    )

import Http.Detailed
import Json.Decode as Decode
import Model
import Routing as R


httpHumanError : Http.Detailed.Error String -> String
httpHumanError err =
    case err of
        Http.Detailed.Timeout ->
            "Something is taking too long."

        Http.Detailed.NetworkError ->
            "Oops. There's been a network error."

        Http.Detailed.BadStatus metadata body ->
            case Decode.decodeString bodyErrorDecoder body of
                Ok errorString ->
                    "Bad status - " ++ String.fromInt metadata.statusCode ++ " " ++ metadata.statusText ++ " - " ++ errorString
                Err decodingError ->
                    "Bad status - " ++ String.fromInt metadata.statusCode ++ " " ++ metadata.statusText ++ " - " ++ body ++ " - JSON decoding error - " ++ Decode.errorToString decodingError

        Http.Detailed.BadBody metadata body s ->
            "Bad payload - " ++ String.fromInt metadata.statusCode ++ " " ++ metadata.statusText ++ " - body - " ++ body ++ " - JSON decoding error - " ++ s

        Http.Detailed.BadUrl url ->
            "Bad url - " ++ url


checkAccess : R.Route -> Model.JwtPayload -> R.Route
checkAccess route jwt =
    case route of
        R.AccessDeniedRoute ->
            route

        R.NotFoundRoute ->
            route

        R.LoginRoute ->
            route

        R.HomeRoute ->
            route

        R.GameRouteVs ->
            route

        R.GameRouteDp ->
            route

        R.GameRouteGn ->
            route

        R.GameRouteSs ->
            route

        R.BadgesRoute ->
            route

        R.SettingsRoute ->
            route

        R.InstructionsRoute ->
            route

        R.StatementsRoute ->
            route

        R.AdminRoute ->
            if isAdmin jwt || isStaff jwt then
                route

            else
                R.AccessDeniedRoute

        R.RegisterRoute ->
            route

        R.EditUserRoute _ ->
            route

        R.MesRoute ->
            route

        R.FmriRoute _ ->
            if isAdmin jwt || isStaff jwt then
                route

            else
                R.AccessDeniedRoute


getJwt : Model.Visitor -> Maybe Model.JwtPayload
getJwt visitor =
    case visitor of
        Model.Anon ->
            Nothing

        Model.LoggedIn jwt ->
            Just jwt


isAdmin : Model.JwtPayload -> Bool
isAdmin jwt =
    List.map .name jwt.roles |> List.member "admin"


isStaff : Model.JwtPayload -> Bool
isStaff jwt =
    List.map .name jwt.roles |> List.member "staff"


keyDecoder : Decode.Decoder String
keyDecoder =
  Decode.field "key" Decode.string


bodyErrorDecoder : Decode.Decoder String
bodyErrorDecoder =
  Decode.field "error" Decode.string
