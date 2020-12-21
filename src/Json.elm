module Json exposing
    ( authDecoder
    , badgeRulesDecoder
    , badgesDecoder
    , cycleDecoder
    , cycleEncoder
    , cyclesEncoder
    , loginEncoder
    , mesEncoder
    , putSessionEncoder
    , sessionDecoder
    , sessionEncoder
    , userEncoder
    )

import Game
import Json.Decode as JD exposing (Decoder)
import Json.Decode.Pipeline exposing (hardcoded, optional, required)
import Json.Encode as JE exposing (object)
import Json.Encode.Extra
import Model as M
import String
import Time


sessionDecoder : Decoder Game.Session
sessionDecoder =
    JD.succeed Game.Session
        |> required "id" JD.string
        |> required "userId" JD.string
        |> required "gameId" JD.string
        |> optional "seed" JD.int 0
        |> required "start" decodeTime
        |> optional "end" (JD.map Just decodeTime) Nothing
        |> optional "jitter" JD.bool False


cycleDecoder : Decoder Game.Cycle
cycleDecoder =
    JD.succeed Game.Cycle
        |> required "id" (JD.maybe JD.string)
        |> required "gsessionId" JD.string
        |> optional "sort" JD.int 0
        |> optional "fixation" (JD.map Just decodeTime) Nothing
        |> optional "selection" (JD.map Just decodeTime) Nothing
        |> optional "pictures" (JD.map Just decodeTime) Nothing
        |> optional "redcross" (JD.map Just decodeTime) Nothing
        |> optional "probe" (JD.map Just decodeTime) Nothing
        |> optional "border" (JD.map Just decodeTime) Nothing
        |> optional "timeout" (JD.map Just decodeTime) Nothing
        |> optional "rest" (JD.map Just decodeTime) Nothing
        |> optional "interval" (JD.map Just decodeTime) Nothing
        |> optional "width" (JD.int |> JD.map numberToMaybe) Nothing
        |> optional "height" (JD.int |> JD.map numberToMaybe) Nothing
        |> optional "blue" JD.bool False
        |> optional "gray" JD.bool False
        |> optional "dash" JD.bool False
        |> optional "targetIndex" JD.int 0
        |> optional "selectedIndex" JD.int 0
        |> optional "startIndex" JD.int 0
        |> optional "ugimageIds" (JD.list JD.string) []


sessionEncoder : { a | userId : String, gameId : String, start : Time.Posix, end : Maybe Time.Posix, seed : Int, jitter : Bool } -> JE.Value
sessionEncoder { userId, gameId, start, end, seed, jitter } =
    object
        [ ( "userId", userId |> JE.string )
        , ( "gameId", gameId |> JE.string )
        , ( "seed", seed |> String.fromInt |> JE.string )
        , ( "start", start |> Time.posixToMillis |> String.fromInt |> JE.string )
        , ( "end", end |> Json.Encode.Extra.maybe encodeTime )
        , ( "jitter", jitter |> JE.bool )
        ]


putSessionEncoder : Game.Session -> JE.Value
putSessionEncoder session =
    object
        [ ( "gsessionId", session.id |> JE.string )
        , ( "gsessionReq", sessionEncoder session )
        ]


cyclesEncoder : Game.Session -> List Game.Cycle -> JE.Value
cyclesEncoder session cycles =
    object
        [ ( "gsessionId", session.id |> JE.string )
        , ( "gcycles", cycles |> List.map cycleEncoder |> JE.list )
        ]


cycleEncoder : Game.Cycle -> JE.Value
cycleEncoder cycle =
    object
        [ ( "gsessionId", cycle.sessionId |> JE.string )
        , ( "sort", cycle.sort |> JE.int )
        , ( "fixation", cycle.fixation |> Json.Encode.Extra.maybe encodeTime )
        , ( "selection", cycle.selection |> Json.Encode.Extra.maybe encodeTime )
        , ( "pictures", cycle.pictures |> Json.Encode.Extra.maybe encodeTime )
        , ( "redcross", cycle.redcross |> Json.Encode.Extra.maybe encodeTime )
        , ( "probe", cycle.probe |> Json.Encode.Extra.maybe encodeTime )
        , ( "border", cycle.border |> Json.Encode.Extra.maybe encodeTime )
        , ( "timeout", cycle.timeout |> Json.Encode.Extra.maybe encodeTime )
        , ( "rest", cycle.rest |> Json.Encode.Extra.maybe encodeTime )
        , ( "interval", cycle.interval |> Json.Encode.Extra.maybe encodeTime )
        , ( "width", cycle.width |> Maybe.withDefault 1 |> JE.int )
        , ( "height", cycle.height |> Maybe.withDefault 1 |> JE.int )
        , ( "blue", cycle.blue |> JE.bool )
        , ( "gray", cycle.gray |> JE.bool )
        , ( "dash", cycle.dash |> JE.bool )
        , ( "targetIndex", cycle.targetIndex |> JE.int )
        , ( "selectedIndex", cycle.selectedIndex |> JE.int )
        , ( "startIndex", cycle.startIndex |> JE.int )
        , ( "ugimageIds", cycle.images |> List.map JE.string |> JE.list )
        ]


mesEncoder : M.MesAnswer -> String -> JE.Value
mesEncoder mes sub =
    JE.object
        [ ( "mesanswerReq"
          , JE.object
                [ ( "userId", JE.string sub )
                , ( "mesqueryId", JE.string mes.queryId )
                , ( "content", JE.string mes.essay )
                , ( "public", JE.bool mes.public )
                ]
          )
        ]


userEncoder : M.UserEdit -> JE.Value
userEncoder u_ =
    JE.object
        [ ( "userReq"
          , JE.object
                [ ( "username", JE.string u_.username )
                , ( "email", JE.string u_.email )
                , ( "firstName", JE.string u_.firstName )
                , ( "lastName", JE.string u_.lastName )
                , ( "groupId", JE.string u_.groupId )
                , ( "mesOptin", JE.bool u_.mesOptin )
                , ( "password", JE.string u_.password )
                ]
          )
        ]


loginEncoder : M.Login -> JE.Value
loginEncoder l_ =
    JE.object
        [ ( "username", JE.string l_.username )
        , ( "password", JE.string l_.password )
        ]


authDecoder : Decoder String
authDecoder =
    JD.field "token" JD.string


badgeRulesDecoder : Decoder (List M.BadgeRule)
badgeRulesDecoder =
    JD.field "badgerules" (JD.list badgeRuleDecoder)


badgeRuleDecoder : Decoder M.BadgeRule
badgeRuleDecoder =
    JD.succeed M.BadgeRule
        |> required "id" JD.string
        |> required "name" JD.string
        |> required "dscript" JD.string


badgesDecoder : Decoder (List String)
badgesDecoder =
    JD.field "badges" (JD.list (JD.field "badgeruleId" JD.string))



-- HELPERS


numberToMaybe : number -> Maybe number
numberToMaybe number =
    case number of
        0 ->
            Nothing

        _ ->
            Just number


decodeTime : Decoder Time.Posix
decodeTime =
    JD.string
        |> JD.andThen
            (\val ->
                case String.toInt val of
                    Just ms ->
                        JD.succeed <| Time.millisToPosix ms
                    Nothing ->
                        JD.fail ("Failed to decode as Time.Posix : \"" ++ val ++ "\"")
            )

encodeTime : Time.Posix -> JE.Value
encodeTime =
    Time.posixToMillis >> JE.int