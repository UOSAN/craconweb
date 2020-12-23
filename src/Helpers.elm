module Helpers exposing (..)

import Http
import Json.Decode as Decode
import Model
import Routing as R


httpHumanError : Http.Error -> String
httpHumanError err =
    case err of
        Http.Timeout ->
            "Something is taking too long."

        Http.NetworkError ->
            "Oops. There's been a network error."

        Http.BadStatus statusCode ->
            "Bad status " ++ String.fromInt statusCode

        Http.BadBody _ ->
            "Bad payload"

        _ ->
            "Unknown error"


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