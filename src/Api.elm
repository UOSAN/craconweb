module Api exposing
    ( createAuthRecord
    , createMesAnswer
    , createUserRecord
    , endSession
    , fetchAll
    , fetchFiller
    , fetchFmriUserData
    , fetchGame
    , fetchGroup
    , fetchInvalid
    , fetchMesAuthors
    , fetchMesQuerys
    , fetchPublicMesAnswers
    , fetchRole
    , fetchUser
    , fetchUsers
    , fetchUsers_
    , fetchValid
    , jwtDecoded
    , okyToky
    , postCycles
    , startSession
    , updateMesStatus
    , updateUser
    )

import Entity
import Game
import Helpers exposing (isAdmin, isStaff)
import Http
import Json
import Json.Decode as JD
import Json.Encode as JE
import Jwt
import Model as M
import RemoteData
import Routing as R
import Task exposing (Task)
import Time



{-
   The Api module is primarily for
   fetching, updating and creating
   server side resources
-}


fetchAll : String -> M.JwtPayload -> String -> Cmd M.Msg
fetchAll httpsrv jwt token =
    case isAdmin jwt || isStaff jwt of
        True ->
            Cmd.batch <|
                adminOnly httpsrv token
                    ++ shared httpsrv token jwt.sub

        False ->
            Cmd.batch <|
                shared httpsrv token jwt.sub


shared : String -> String -> String -> List (Cmd M.Msg)
shared httpsrv token sub =
    [ Task.attempt M.GameResp (fetchGame httpsrv token "gonogo")
    , Task.attempt M.GameResp (fetchGame httpsrv token "dotprobe")
    , Task.attempt M.GameResp (fetchGame httpsrv token "stopsignal")
    , Task.attempt M.GameResp (fetchGame httpsrv token "visualsearch")
    , Task.attempt M.UserResp (fetchUser httpsrv token sub)
    , Task.attempt M.PublicMesResp (fetchPublicMesAnswers { url = httpsrv, token = token, sub = sub })
    , Task.attempt M.FillerResp (fetchFiller httpsrv token sub)
    , Task.attempt M.ValidResp (fetchValid httpsrv token sub)
    , Task.attempt M.InvalidResp (fetchInvalid httpsrv token sub)
    , Task.attempt M.MesAnswersResp (fetchMesAnswersByUser { url = httpsrv, token = token, sub = sub })
    , fetchBadgeRules { url = httpsrv, token = token, sub = sub }
    , fetchBadgesByUserId { url = httpsrv, token = token, sub = sub }
    , Task.attempt M.MesQuerysResp (fetchMesQuerys { url = httpsrv, token = token, sub = sub })
    ]


adminOnly : String -> String -> List (Cmd M.Msg)
adminOnly httpsrv token =
    [ Task.attempt M.UsersResp (fetchUsers_ httpsrv token)
    , Task.attempt M.RoleResp (fetchRole httpsrv token "user")
    , Task.attempt M.GroupResp (fetchGroup httpsrv token "control_a")
    , Task.attempt M.GroupResp (fetchGroup httpsrv token "experimental_a")
    , Task.attempt M.MesResp (fetchMesAnswers { url = httpsrv, token = token, sub = "" })
    ]


defaultHeaders : String -> List Http.Header
defaultHeaders jwtencoded =
    let
        headers =
            [ Http.header "Accept" "application/json" ]

        authHeaders =
            case jwtencoded of
                "" ->
                    headers

                _ ->
                    Http.header "Authorization" ("Bearer " ++ jwtencoded) :: headers
    in
    authHeaders


updateMesStatus : M.Base -> String -> M.MesAnswer -> Task Http.Error String
updateMesStatus { url, token, sub } id updatedMes =
    putRequest
        { endpoint = url ++ "/mesanswer/" ++ id
        , decoder = JD.succeed "Saved."
        , token = token
        , json = Json.mesEncoder updatedMes sub
        }


updateUser : M.Base -> M.UserEdit -> Task Http.Error String
updateUser { url, token, sub } user =
    putRequest
        { endpoint = url ++ "/user/" ++ user.id
        , decoder = JD.succeed "Saved."
        , token = token
        , json = Json.userEncoder user
        }


createMesAnswer : M.Base -> M.MesAnswer -> String -> Task Http.Error String
createMesAnswer b answer sub =
    Http.task
        { method = "POST"
        , headers = defaultHeaders b.token
        , url = b.url ++ "/user/" ++ sub ++ "/mesquery/" ++ answer.queryId ++ "/mesanswer"
        , body = Http.jsonBody <| M.mesAnswerEncoder answer
        , resolver = Http.stringResolver <| responseToResult
        , timeout = Nothing
        }


fetchMesAnswers : M.Base -> Task Http.Error (List M.MesAnswer)
fetchMesAnswers b =
    getRequest b.token (b.url ++ "/mesanswers?userEach=true&groupEach=true&createdEach=true&optin=true&public=false") M.mesAnswersDecoder


answersToParams : List M.MesAnswer -> String
answersToParams answers =
    "mesanswerIds="
        ++ (answers
                |> List.map .id
                |> List.filter (\a -> a /= "")
                |> String.join "&mesanswerIds="
           )


fetchMesAuthors : M.Base -> List M.MesAnswer -> String -> Task Http.Error (List M.MesAuthor)
fetchMesAuthors b answers groupId =
    getRequest b.token
        (b.url
            ++ "/mesauthors?groupId="
            ++ groupId
            ++ "&"
            ++ answersToParams answers
        )
        M.mesAuthorsDecoder


fetchBadgeRules : M.Base -> Cmd M.Msg
fetchBadgeRules { url, token, sub } =
    Http.request
        { method = "GET"
        , headers = defaultHeaders token
        , url = url ++ "/badgerules?sortEach=true"
        , body = Http.emptyBody
        , expect = Http.expectJson (RemoteData.fromResult >> M.BadgeRulesResp) Json.badgeRulesDecoder
        , timeout = Nothing
        , tracker = Nothing
        }


fetchBadgesByUserId : M.Base -> Cmd M.Msg
fetchBadgesByUserId { url, token, sub } =
    Http.request
        { method = "GET"
        , headers = defaultHeaders token
        , url = url ++ "/user/" ++ sub ++ "/badges"
        , body = Http.emptyBody
        , expect = Http.expectJson (RemoteData.fromResult >> M.BadgesResp) Json.badgesDecoder
        , timeout = Nothing
        , tracker = Nothing
        }


fetchPublicMesAnswers : M.Base -> Task Http.Error (List M.MesAnswer)
fetchPublicMesAnswers b =
    getRequest b.token (b.url ++ "/mesanswers?userEach=true&groupEach=true&createdEach=true&public=true&optin=true") M.mesAnswersDecoder


fetchMesAnswersByUser : M.Base -> Task Http.Error (List M.MesAnswer)
fetchMesAnswersByUser { url, token, sub } =
    getRequest token (url ++ "/mesanswers?userId=" ++ sub ++ "&groupEach=true&createdEach=true&optinEach=true&publicEach=true&createdDesc=true") M.mesAnswersDecoder


fetchMesQuerys : M.Base -> Task Http.Error (List M.MesQuery)
fetchMesQuerys b =
    getRequest b.token (b.url ++ "/mesquerys?sortEach=true") M.mesQuerysDecoder


createAuthRecord :
    String
    -> M.Login
    -> Task Http.Error String
createAuthRecord httpsrv login =
    Http.task
        { method = "POST"
        , headers = []
        , url = httpsrv ++ "/auth"
        , body = Http.jsonBody <| Json.loginEncoder login
        , resolver = Http.stringResolver <| handleJsonResponse <| Json.authDecoder
        , timeout = Nothing
        }


fetchAllUgimgsets :
    String
    -> String
    -> String
    -> Task Http.Error (List Entity.Ugimgset)
fetchAllUgimgsets httpsrv token sub =
    getRequest token
        (httpsrv
            ++ "/user/"
            ++ sub
            ++ "/ugimgsets?createdDesc=true&createdEach=true"
        )
        M.ugimgsetsDecoder


fetchUsers_ : String -> String -> Task Http.Error (List Entity.User)
fetchUsers_ httpsrv token =
    fetchRole httpsrv token "user"
        |> Task.andThen (fetchUsersInRole httpsrv token << .id)


fetchFiller :
    String
    -> String
    -> String
    -> Task M.ValuationsError (List Entity.Ugimage)
fetchFiller httpsrv token sub =
    fetchImages httpsrv token sub "40" "filler"


fetchValid :
    String
    -> String
    -> String
    -> Task M.ValuationsError (List Entity.Ugimage)
fetchValid httpsrv token sub =
    fetchImages httpsrv token sub "80" "valid"


fetchInvalid :
    String
    -> String
    -> String
    -> Task M.ValuationsError (List Entity.Ugimage)
fetchInvalid httpsrv token sub =
    fetchImages httpsrv token sub "80" "invalid"


fetchImages :
    String
    -> String
    -> String
    -> String
    -> String
    -> Task M.ValuationsError (List Entity.Ugimage)
fetchImages httpsrv token sub count kind =
    fetchUgimgsets httpsrv token sub
        |> Task.mapError M.ReqFail
        |> Task.andThen
            (\ugimgsets ->
                case ugimsetsToString ugimgsets of
                    Just id ->
                        fetchUgimages httpsrv token count kind id
                            |> Task.mapError M.ReqFail

                    Nothing ->
                        Task.fail M.MissingValuations
            )


ugimsetsToString : List Entity.Ugimgset -> Maybe String
ugimsetsToString ugimgset =
    ugimgset
        |> List.head
        |> Maybe.map .id


fetchUgimages :
    String
    -> String
    -> String
    -> String
    -> String
    -> Task Http.Error (List Entity.Ugimage)
fetchUgimages httpsrv token limit gimgtypeSlug ugimgsetId =
    getRequest token
        (httpsrv
            ++ "/ugimgset/"
            ++ ugimgsetId
            ++ "/ugimages?ratingDesc=true&limit="
            ++ limit
            ++ "&ratingEach=true&gimgtypeSlug="
            ++ gimgtypeSlug
        )
        M.ugimageDecoder


fetchUgimgsets :
    String
    -> String
    -> String
    -> Task Http.Error (List Entity.Ugimgset)
fetchUgimgsets httpsrv token sub =
    getRequest token
        (httpsrv
            ++ "/user/"
            ++ sub
            ++ "/ugimgsets?createdDesc=true&limit=1&createdEach=true"
        )
        M.ugimgsetsDecoder


fetchGame : String -> String -> String -> Task Http.Error Entity.Game
fetchGame httpsrv token slug =
    getRequest token (httpsrv ++ "/game/" ++ slug) Entity.gameDecoder


fetchUser : String -> String -> String -> Task Http.Error Entity.User
fetchUser httpsrv token sub =
    getRequest token (httpsrv ++ "/user/" ++ sub) Entity.userDecoder


fetchUsers : String -> String -> Task Http.Error (List Entity.User)
fetchUsers httpsrv token =
    getRequest token
        (httpsrv ++ "/users?createdEach=true")
        (JD.field "users" (JD.list Entity.userDecoder))


fetchUserImages : String -> String -> String -> Cmd M.Msg
fetchUserImages httpsrv token userId =
    fetchUser httpsrv token userId
        |> Task.mapError M.ReqFail
        |> Task.andThen
            (\user ->
                Task.map3 (\f v i -> { user = user, ugimages_f = f, ugimages_v = v, ugimages_i = i })
                    (fetchFiller httpsrv token userId)
                    (fetchValid httpsrv token userId)
                    (fetchInvalid httpsrv token userId)
            )
        |> Task.attempt (RemoteData.fromResult >> M.FmriImagesResp)


fetchFmriUserData : M.Model -> ( M.Model, Cmd M.Msg )
fetchFmriUserData model =
    case model.activeRoute of
        R.FmriRoute userId ->
            ( { model | fmriUserData = RemoteData.Loading }
            , fetchUserImages model.httpsrv model.jwtencoded userId
            )

        _ ->
            ( model, Cmd.none )


fetchUsersInRole :
    String
    -> String
    -> String
    -> Task Http.Error (List Entity.User)
fetchUsersInRole httpsrv token roleId =
    getRequest token
        (httpsrv ++ "/users?createdEach=true&roleId=" ++ roleId)
        (JD.field "users" (JD.list Entity.userDecoder))


fetchGroup : String -> String -> String -> Task Http.Error Entity.Group
fetchGroup httpsrv token slug =
    getRequest token (httpsrv ++ "/group/" ++ slug) Entity.groupDecoder


fetchRole : String -> String -> String -> Task Http.Error Entity.Role
fetchRole httpsrv token slug =
    getRequest token (httpsrv ++ "/role/" ++ slug) Entity.roleDecoder


createUserRecord :
    String
    -> String
    -> Entity.UserRecord
    -> Task Http.Error Entity.User
createUserRecord httpsrv token user =
    Http.task
        { method = "POST"
        , headers = defaultHeaders token
        , url = httpsrv ++ "/user"
        , body = Http.jsonBody <| Entity.userRecordEncoder user
        , resolver = Http.stringResolver <| handleJsonResponse <| Entity.userDecoder
        , timeout = Nothing
        }


okyToky : Time.Posix -> String -> Result String M.JwtPayload
okyToky now token =
    case Jwt.decodeToken M.jwtDecoder token of
        Ok decoded ->
            case Jwt.isExpired now token of
                Ok True ->
                    Err "Expired"

                Ok False ->
                    Ok decoded

                Err jwtErr ->
                    Err "Decoding Problem"

        _ ->
            Err "Decoding problem"


jwtDecoded : String -> Result Jwt.JwtError M.JwtPayload
jwtDecoded token =
    Jwt.decodeToken M.jwtDecoder token


startSession : { token : String, userId : String, gameId : String, start : Time.Posix, httpsrv : String, initialSeed : Int, jitter : Bool } -> Task Never (RemoteData.WebData Game.Session)
startSession { token, userId, gameId, start, httpsrv, initialSeed, jitter } =
    let
        json =
            Json.sessionEncoder
                { userId = userId
                , gameId = gameId
                , seed = initialSeed
                , start = start
                , end = Nothing
                , jitter = jitter
                }
    in
    postRequest
        { endpoint = httpsrv ++ "/user/" ++ userId ++ "/gsession"
        , decoder = Json.sessionDecoder
        , token = token
        , json = json
        }
        |> RemoteData.fromTask


endSession : { session : Game.Session, token : String, httpsrv : String } -> Task Never (RemoteData.WebData Game.Session)
endSession { session, token, httpsrv } =
    let
        json =
            Json.putSessionEncoder session
    in
    putRequest
        { endpoint = httpsrv ++ "/gsession/" ++ session.id
        , decoder = Json.sessionDecoder
        , token = token
        , json = json
        }
        |> RemoteData.fromTask


postCycles : { session : Game.Session, cycles : List Game.Cycle, token : String, httpsrv : String } -> Task Never (RemoteData.WebData (List Game.Cycle))
postCycles { session, cycles, token, httpsrv } =
    let
        json =
            Json.cyclesEncoder session cycles
    in
    postRequest
        { endpoint = httpsrv ++ "/gsession/" ++ session.id ++ "/gcycles"
        , decoder = JD.at [ "gcycles" ] (JD.list Json.cycleDecoder)
        , token = token
        , json = json
        }
        |> RemoteData.fromTask


postRequest : { endpoint : String, token : String, decoder : JD.Decoder a, json : JE.Value } -> Task Http.Error a
postRequest { endpoint, decoder, token, json } =
    Http.task
        { method = "POST"
        , headers = defaultHeaders token
        , url = endpoint
        , body = json |> Http.jsonBody
        , resolver = Http.stringResolver <| handleJsonResponse <| decoder
        , timeout = Nothing
        }


putRequest : { endpoint : String, token : String, decoder : JD.Decoder a, json : JE.Value } -> Task Http.Error a
putRequest { endpoint, decoder, token, json } =
    Http.task
        { method = "PUT"
        , headers = defaultHeaders token
        , url = endpoint
        , body = json |> Http.jsonBody
        , resolver = Http.stringResolver <| handleJsonResponse <| decoder
        , timeout = Nothing
        }


getRequest : String -> String -> JD.Decoder a -> Task Http.Error a
getRequest token endpoint decoder =
    Http.task
        { method = "GET"
        , headers = defaultHeaders token
        , url = endpoint
        , body = Http.emptyBody
        , resolver = Http.stringResolver <| handleJsonResponse <| decoder
        , timeout = Nothing
        }


-- HELPERS
{-| Decode a JSON response, while handling possible errors.
-}
handleJsonResponse : JD.Decoder a -> Http.Response String -> Result Http.Error a
handleJsonResponse decoder response =
    case response of
        Http.BadUrl_ url ->
            Err (Http.BadUrl url)

        Http.Timeout_ ->
            Err Http.Timeout

        Http.BadStatus_ { statusCode } _ ->
            Err (Http.BadStatus statusCode)

        Http.NetworkError_ ->
            Err Http.NetworkError

        Http.GoodStatus_ _ body ->
            case JD.decodeString decoder body of
                Ok result ->
                    Ok result

                Err err ->
                    Err (Http.BadBody ((JD.errorToString err) ++ body))


{-| Handle a string response, while handling possible errors.
-}
responseToResult : Http.Response a -> Result Http.Error a
responseToResult response =
    case response of
        Http.BadUrl_ url ->
            Err (Http.BadUrl url)

        Http.Timeout_ ->
            Err Http.Timeout

        Http.BadStatus_ { statusCode } _ ->
            Err (Http.BadStatus statusCode)

        Http.NetworkError_ ->
            Err Http.NetworkError

        Http.GoodStatus_ _ body ->
            Ok body