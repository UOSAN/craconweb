module Model exposing (..)

import Browser.Navigation
import Entity
import Game
import Http
import Json.Decode as JD
import Json.Decode.Pipeline as JP
import Json.Encode as JE
import List.Extra as LE
import Random
import RemoteData
import Routing
import Time
import Url
import Window as W


type alias Flags =
    { token : String
    , time : Float
    }


type alias Model =
    { httpsrv : String
    , tasksrv : String
    , filesrv : String
    , jwtencoded : String
    , activeRoute : Routing.Route
    , visitor : Visitor
    , isMenuActive : Bool
    , user : Maybe Entity.User
    , login : Login
    , ugimages_v : Maybe (List Entity.Ugimage)
    , ugimages_i : Maybe (List Entity.Ugimage)
    , ugimages_f : Maybe (List Entity.Ugimage)
    , loading : Maybe String
    , glitching : Maybe String
    , informing : Maybe String
    , users : List Entity.User
    , userRole : Entity.Role
    , groupIdExp : Maybe String
    , groupIdCon : Maybe String
    , httpErr : String
    , gonogoGame : Maybe Entity.Game
    , dotprobeGame : Maybe Entity.Game
    , stopsignalGame : Maybe Entity.Game
    , respondsignalGame : Maybe Entity.Game
    , visualsearchGame : Maybe Entity.Game
    , gameState : Game.GameState Msg
    , ugimgsets : Maybe (List Entity.Ugimgset)
    , mesQuery : Maybe String
    , mesQuerys : Maybe (List MesQuery)
    , mesAnswers : Maybe (List MesAnswer)
    , mesAnswer : Maybe MesAnswer
    , adminModel : AdminModel
    , statements : Maybe (List MesAnswer)
    , request : Maybe String
    , loadTime : Time.Time
    , badgeRules : RemoteData.WebData (List BadgeRule)
    , domLoaded : Bool
    , badgesEarned : RemoteData.WebData (List String)
    , fmriUserData : RemoteData.RemoteData ValuationsError FmriUserData
    , statementsModal : Bool
    , windowSize : Maybe W.Size
    , key : Browser.Navigation.Key
    }


type alias FmriUserData =
    { user : Entity.User
    , ugimages_v : List Entity.Ugimage
    , ugimages_i : List Entity.Ugimage
    , ugimages_f : List Entity.Ugimage
    }


type Msg
    = UpdateLocation String
    | OnUpdateLocation Url.Url
    | FillTmpUserEdit String
    | GroupChanged (Maybe String)
    | SetStatus String
    | UpdateEmail String
    | UpdatePassword String
    | TryLogin
    | Logout
    | ResetNotifications
    | MainMenuToggle
    | NewCurrentTime Time.Posix
    | Presses Int
    | SelectInput Int
    | DirectionInput Game.Direction
    | IndicationInput
    | InitStopSignal
    | InitFmriStopSignal { user : Entity.User }
    | InitGoNoGo
    | InitDotProbe
    | InitVisualSearch
    | StartSession { gameId : String, game : Game.Game Msg, time : Time.Time, initialSeed : Int, nextSeed : Random.Seed }
    | StartSessionResp Random.Seed (Game.Game Msg) (RemoteData.WebData Game.Session)
    | GameDataSaved Game.State Game.Session (RemoteData.WebData ( Game.Session, List Game.Cycle ))
    | ResendSession Game.State Game.Session
    | AuthResp (Result Http.Error String)
    | PublicMesResp (Result Http.Error (List MesAnswer))
    | MesAuthorsResp (Result Http.Error (List MesAuthor))
    | UserResp (Result Http.Error Entity.User)
    | GameResp (Result Http.Error Entity.Game)
    | UsersResp (Result Http.Error (List Entity.User))
    | RegisterUserResp (Result Http.Error Entity.User)
    | EditUserResp (Result Http.Error Entity.User)
    | GroupResp (Result Http.Error Entity.Group)
    | MesResp (Result Http.Error (List MesAnswer))
    | MesPostResp (Result Http.Error String)
    | PutMesResp (Result Http.Error String)
    | UserEditResp (Result Http.Error String)
    | MesQuerysResp (Result Http.Error (List MesQuery))
    | MesAnswersResp (Result Http.Error (List MesAnswer))
    | RoleResp (Result Http.Error Entity.Role)
    | FillerResp (Result ValuationsError (List Entity.Ugimage))
    | ValidResp (Result ValuationsError (List Entity.Ugimage))
    | InvalidResp (Result ValuationsError (List Entity.Ugimage))
    | BadgeRulesResp (RemoteData.WebData (List BadgeRule))
    | BadgesResp (RemoteData.WebData (List String))
    | ToggleStatementsModal
    | TryRegisterUser
    | SetRegistration String String
    | ToggleRegistrationMesOptin
    | TryCsvUpload
    | TryPutUser
    | EditUserAccount String String
    | PublishMes String
    | UpdateMesAnswer String
    | TrySubmitMesAnswer
    | SetRequestNothing
    | SetTmpUserEdit String String
    | ToggleTmpUserEditMesOptin
    | DomLoaded Bool
    | FmriImagesResp (RemoteData.RemoteData ValuationsError FmriUserData)
    | WindowResize W.Size


type alias Login =
    { username : String
    , password : String
    }


type alias BadgeRule =
    { id : String
    , name : String
    , dscript : String
    }


type alias AdminModel =
    { tmpUserRecord : Entity.UserRecord
    , mesAnswers : Maybe (List MesAnswer)
    , tmpUserEdit : Maybe UserEdit
    }


type alias UserEdit =
    { id : String
    , username : String
    , firstName : String
    , lastName : String
    , email : String
    , password : String
    , groupId : String
    , mesOptin : Bool
    }


up_mesAnswers : AdminModel -> List MesAnswer -> AdminModel
up_mesAnswers am mes =
    { am | mesAnswers = Just mes }


up_tmpUserRecord : AdminModel -> Entity.UserRecord -> AdminModel
up_tmpUserRecord am tur =
    { am | tmpUserRecord = tur }


up_tmpUserEdit : AdminModel -> Maybe UserEdit -> AdminModel
up_tmpUserEdit am tue =
    { am | tmpUserEdit = tue }


up_mesAnswersDisplayName : List MesAuthor -> MesAnswer -> MesAnswer
up_mesAnswersDisplayName authors ans =
    case LE.find (\auth -> auth.answerId == ans.id) authors of
        Just auth ->
            { ans | displayName = auth.userName }

        Nothing ->
            ans


type alias MesAuthor =
    { answerId : String
    , userName : String
    }


mesAuthorsDecoder : JD.Decoder (List MesAuthor)
mesAuthorsDecoder =
    JD.field "mesauthors" (JD.list mesAuthorDecoder)


mesAuthorDecoder : JD.Decoder MesAuthor
mesAuthorDecoder =
    JD.succeed MesAuthor
        |> JP.required "mesanswerId" JD.string
        |> JP.required "userName" JD.string


type alias MesAnswer =
    { id : String
    , essay : String
    , public : Bool
    , queryId : String
    , displayName : String
    , userId : String
    , created : String
    }


newMesAnswerWithqueryId : String -> MesAnswer
newMesAnswerWithqueryId qId =
    { id = ""
    , essay = ""
    , public = False
    , queryId = qId
    , displayName = ""
    , userId = ""
    , created = ""
    }


up_essay : String -> MesAnswer -> MesAnswer
up_essay essay ma =
    { ma | essay = essay }


mesAnswersDecoder : JD.Decoder (List MesAnswer)
mesAnswersDecoder =
    JD.field "mesanswers" (JD.list mesAnswerDecoder)


mesAnswerDecoder : JD.Decoder MesAnswer
mesAnswerDecoder =
    JD.succeed MesAnswer
        |> JP.required "id" JD.string
        |> JP.required "content" JD.string
        |> JP.optional "public" JD.bool False
        |> JP.required "mesqueryId" JD.string
        |> JP.required "initials" JD.string
        |> JP.optional "userId" JD.string ""
        |> JP.required "created" JD.string


mesAnswerEncoder : MesAnswer -> JE.Value
mesAnswerEncoder mesAnswer =
    JE.object [ ( "content", JE.string mesAnswer.essay ) ]


type alias MesQuery =
    { id : String
    , content : String
    }


mesQueryDecoder : JD.Decoder MesQuery
mesQueryDecoder =
    JD.succeed MesQuery
        |> JP.required "id" JD.string
        |> JP.required "content" JD.string


mesQuerysDecoder : JD.Decoder (List MesQuery)
mesQuerysDecoder =
    JD.field "mesquerys" (JD.list mesQueryDecoder)


type alias Base =
    { url : String
    , token : String
    , sub : String
    }


type ValuationsError
    = ReqFail Http.Error
    | MissingValuations


type Visitor
    = Anon
    | LoggedIn JwtPayload


type alias JwtPayload =
    { aud : String
    , exp : Float
    , iat : Int
    , iss : String
    , sub : String
    , roles : List Entity.Role
    , groupId : String
    }


ugimgsetsDecoder : JD.Decoder (List Entity.Ugimgset)
ugimgsetsDecoder =
    JD.field "ugimgsets" (JD.list Entity.ugimgsetDecoder)
        |> JD.maybe
        |> JD.map (Maybe.withDefault [])


ugimageDecoder : JD.Decoder (List Entity.Ugimage)
ugimageDecoder =
    JD.field "ugimages" (JD.list Entity.ugimageDecoder)


type alias ErrorCode =
    { error : String
    , code : Int
    }


errorCodeEncoder : String -> ErrorCode
errorCodeEncoder errorCode =
    case JD.decodeString errorCodeDecoder errorCode of
        Ok ed ->
            ed

        Err _ ->
            { error = ""
            , code = 0
            }


tokenEncoder : String -> JE.Value
tokenEncoder token =
    JE.object [ ( "token", JE.string token ) ]


errorCodeDecoder : JD.Decoder ErrorCode
errorCodeDecoder =
    JD.succeed ErrorCode
        |> JP.required "error" JD.string
        |> JP.required "code" JD.int


jwtDecoder : JD.Decoder JwtPayload
jwtDecoder =
    JD.succeed JwtPayload
        |> JP.required "aud" JD.string
        |> JP.required "exp" JD.float
        |> JP.required "iat" JD.int
        |> JP.required "iss" JD.string
        |> JP.required "sub" JD.string
        |> JP.required "roles" (JD.list Entity.roleDecoder)
        |> JP.optional "groupId" JD.string ""
