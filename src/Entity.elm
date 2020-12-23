module Entity exposing (..)

import Json.Decode as JD
import Json.Encode as JE
import Protobuf exposing (..)


type alias Group =
    { id :
        String

    -- 1
    , name :
        String

    -- 2
    , slug :
        String

    -- 3
    , created :
        Maybe Timestamp

    -- 4
    , updated :
        Maybe Timestamp

    -- 5
    }


groupDecoder : JD.Decoder Group
groupDecoder =
    JD.lazy <|
        \_ ->
            decode Group
                |> required "id" JD.string ""
                |> required "name" JD.string ""
                |> required "slug" JD.string ""
                |> optional "created" timestampDecoder
                |> optional "updated" timestampDecoder


type alias Role =
    { id :
        String

    -- 1
    , name :
        String

    -- 2
    , weight :
        Int

    -- 3
    }


roleDecoder : JD.Decoder Role
roleDecoder =
    JD.lazy <|
        \_ ->
            decode Role
                |> required "id" JD.string ""
                |> required "name" JD.string ""
                |> required "weight" JD.int 0


type alias User =
    { id :
        String

    -- 1
    , subject :
        String

    -- 2
    , username :
        String

    -- 3
    , email :
        String

    -- 4
    , firstName :
        String

    -- 5
    , lastName :
        String

    -- 6
    , avatar :
        String

    -- 7
    , groupId :
        String

    -- 8
    , mesOptin :
        Bool

    -- 9
    , roles :
        List Role

    -- 10
    , lastLogin :
        Maybe Timestamp

    -- 11
    , blocked :
        Maybe Timestamp

    -- 12
    , created :
        Maybe Timestamp

    -- 13
    , updated :
        Maybe Timestamp

    -- 14
    , deleted :
        Maybe Timestamp

    -- 15
    }


userDecoder : JD.Decoder User
userDecoder =
    JD.lazy <|
        \_ ->
            decode User
                |> required "id" JD.string ""
                |> required "subject" JD.string ""
                |> required "username" JD.string ""
                |> required "email" JD.string ""
                |> required "firstName" JD.string ""
                |> required "lastName" JD.string ""
                |> required "avatar" JD.string ""
                |> required "groupId" JD.string ""
                |> required "mesOptin" JD.bool False
                |> repeated "roles" roleDecoder
                |> optional "lastLogin" timestampDecoder
                |> optional "blocked" timestampDecoder
                |> optional "created" timestampDecoder
                |> optional "updated" timestampDecoder
                |> optional "deleted" timestampDecoder


type alias UserRecord =
    { username :
        String

    -- 1
    , email :
        String

    -- 2
    , firstName :
        String

    -- 3
    , lastName :
        String

    -- 4
    , avatar :
        String

    -- 5
    , groupId :
        String

    -- 6
    , mesOptin :
        Bool

    -- 7
    , roles :
        List String

    -- 8
    , password :
        String

    -- 9
    }


userRecordEncoder : UserRecord -> JE.Value
userRecordEncoder v =
    JE.object <|
        List.filterMap identity <|
            [ requiredFieldEncoder "username" JE.string "" v.username
            , requiredFieldEncoder "email" JE.string "" v.email
            , requiredFieldEncoder "firstName" JE.string "" v.firstName
            , requiredFieldEncoder "lastName" JE.string "" v.lastName
            , requiredFieldEncoder "avatar" JE.string "" v.avatar
            , requiredFieldEncoder "groupId" JE.string "" v.groupId
            , requiredFieldEncoder "mesOptin" JE.bool False v.mesOptin
            , repeatedFieldEncoder "roles" JE.string v.roles
            , requiredFieldEncoder "password" JE.string "" v.password
            ]


type alias Game =
    { id :
        String

    -- 1
    , name :
        String

    -- 2
    , slug :
        String

    -- 3
    , dscript :
        String

    -- 4
    , instruct :
        String

    -- 5
    , icon :
        String

    -- 6
    , reactDur :
        Int

    -- 7
    , sessDur :
        Int

    -- 8
    , trialDur :
        Int

    -- 9
    , offsetDur :
        Int

    -- 10
    , fixDur :
        Int

    -- 11
    , fixImg :
        String

    -- 12
    , durInc :
        Int

    -- 13
    , durDec :
        Int

    -- 14
    , incTrigger :
        Int

    -- 15
    , decTrigger :
        Int

    -- 16
    , blocked :
        Maybe Timestamp

    -- 17
    , created :
        Maybe Timestamp

    -- 18
    , updated :
        Maybe Timestamp

    -- 19
    , deleted :
        Maybe Timestamp

    -- 20
    }


gameDecoder : JD.Decoder Game
gameDecoder =
    JD.lazy <|
        \_ ->
            decode Game
                |> required "id" JD.string ""
                |> required "name" JD.string ""
                |> required "slug" JD.string ""
                |> required "dscript" JD.string ""
                |> required "instruct" JD.string ""
                |> required "icon" JD.string ""
                |> required "reactDur" JD.int 0
                |> required "sessDur" JD.int 0
                |> required "trialDur" JD.int 0
                |> required "offsetDur" JD.int 0
                |> required "fixDur" JD.int 0
                |> required "fixImg" JD.string ""
                |> required "durInc" JD.int 0
                |> required "durDec" JD.int 0
                |> required "incTrigger" JD.int 0
                |> required "decTrigger" JD.int 0
                |> optional "blocked" timestampDecoder
                |> optional "created" timestampDecoder
                |> optional "updated" timestampDecoder
                |> optional "deleted" timestampDecoder
                |> JD.map (Debug.log "gameData")


type alias Gimage =
    { id :
        String

    -- 1
    , name :
        String

    -- 2
    , path :
        String

    -- 3
    , gimgtypeId :
        String

    -- 4
    , created :
        Maybe Timestamp

    -- 5
    , updated :
        Maybe Timestamp

    -- 6
    , deleted :
        Maybe Timestamp

    -- 7
    }


gimageDecoder : JD.Decoder Gimage
gimageDecoder =
    JD.lazy <|
        \_ ->
            decode Gimage
                |> required "id" JD.string ""
                |> required "name" JD.string ""
                |> required "path" JD.string ""
                |> required "gimgtypeId" JD.string ""
                |> optional "created" timestampDecoder
                |> optional "updated" timestampDecoder
                |> optional "deleted" timestampDecoder


type alias Ugimgset =
    { id :
        String

    -- 1
    , ref :
        String

    -- 2
    , userId :
        String

    -- 3
    , created :
        Maybe Timestamp

    -- 4
    , updated :
        Maybe Timestamp

    -- 5
    }


ugimgsetDecoder : JD.Decoder Ugimgset
ugimgsetDecoder =
    JD.lazy <|
        \_ ->
            decode Ugimgset
                |> required "id" JD.string ""
                |> required "ref" JD.string ""
                |> required "userId" JD.string ""
                |> optional "created" timestampDecoder
                |> optional "updated" timestampDecoder


type alias Ugimage =
    { id :
        String

    -- 1
    , gimage :
        Maybe Gimage

    -- 2
    , ugimgsetId :
        String

    -- 3
    , val :
        Int

    -- 4
    }


ugimageDecoder : JD.Decoder Ugimage
ugimageDecoder =
    JD.lazy <|
        \_ ->
            decode Ugimage
                |> required "id" JD.string ""
                |> optional "gimage" gimageDecoder
                |> required "ugimgsetId" JD.string ""
                |> required "val" JD.int 0
