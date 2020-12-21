module Empty exposing (..)

import Entity
import Game
import Model
import RemoteData
import Routing
import Time


initialModel : Model.Model
initialModel =
    { httpsrv = ""
    , tasksrv = ""
    , filesrv = ""
    , jwtencoded = ""
    , activeRoute = Routing.HomeRoute
    , visitor = Model.Anon
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
        { tmpUserRecord = emptyUserRecord
        , mesAnswers = Nothing
        , tmpUserEdit = Nothing
        }
    , statements = Nothing
    , request = Nothing
    , loadTime = Time.millisToPosix 0
    , badgeRules = RemoteData.NotAsked
    , domLoaded = False
    , badgesEarned = RemoteData.NotAsked
    , fmriUserData = RemoteData.NotAsked
    , statementsModal = False
    , windowSize = Nothing
    , key = Nothing
    }


emptyModel : Model.Model -> Model.Model
emptyModel model =
    { model
        | activeRoute = Routing.LoginRoute
        , jwtencoded = ""
        , visitor = Model.Anon
        , users = []
        , isMenuActive = False
        , user = Nothing
        , login = { username = "", password = "" }
        , loading = Nothing
        , glitching = Nothing
        , gameState = Game.NotPlaying
        , fmriUserData = RemoteData.NotAsked
        , ugimages_f = Nothing
        , ugimages_v = Nothing
        , ugimages_i = Nothing
    }


emptyUserRecord : Entity.UserRecord
emptyUserRecord =
    { username = ""
    , email = ""
    , firstName = ""
    , lastName = ""
    , avatar = ""
    , groupId = ""
    , mesOptin = True
    , roles = []
    , password = ""
    }


emptyRole : Entity.Role
emptyRole =
    { id = ""
    , name = "user"
    , weight = 0
    }
