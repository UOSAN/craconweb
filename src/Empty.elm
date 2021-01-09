module Empty exposing
    ( emptyModel
    , emptyUserRecord
    , emptyRole
    )

import Entity
import Game
import Model
import RemoteData
import Routing


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
