module Empty exposing (..)

import Entity
import Game
import Model
import Navigation
import RemoteData
import Routing


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
    , statements = Nothing
    , mesQuery = Nothing
    , mesQuerys = Nothing
    , mesAnswers = Nothing
    , mesAnswer = Nothing
    , request = Nothing
    , adminModel =
        { tmpUserRecord = emptyUserRecord
        , mesAnswers = Nothing
        , tmpUserEdit = Nothing
        }
    , loadTime = 0
    , domLoaded = False
    , badgeRules = RemoteData.NotAsked
    , badgesEarned = RemoteData.NotAsked
    , fmriUserData = RemoteData.NotAsked
    , statementsModal = False
    , windowSize = Nothing
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


emptyLocation : Navigation.Location
emptyLocation =
    { href = ""
    , host = ""
    , hostname = ""
    , protocol = ""
    , origin = ""
    , port_ = ""
    , pathname = ""
    , search = ""
    , hash = ""
    , username = ""
    , password = ""
    }


emptyRole : Entity.Role
emptyRole =
    { id = ""
    , name = "user"
    , weight = 0
    }
