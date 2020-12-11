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
import Model as M
import String
import Time


sessionDecoder : Decoder Game.Session
sessionDecoder =
    JD.succeed Game.Session
        |> required "id" JD.string
        |> required "userId" JD.string
        |> required "gameId" JD.string
        |> optional "seed" stringToIntDecoder 0
        |> required "start" stringToFloatDecoder
        |> optional "end" (JD.maybe JD.float) Nothing
        |> optional "jitter" JD.bool False


cycleDecoder : Decoder Game.Cycle
cycleDecoder =
    JD.succeed Game.Cycle
        |> required "id" (JD.maybe JD.string)
        |> required "gsessionId" JD.string
        |> optional "sort" JD.int 0
        |> optional "fixation" (stringToFloatDecoder |> JD.map numberToMaybe) Nothing
        |> optional "selection" (stringToFloatDecoder |> JD.map numberToMaybe) Nothing
        |> optional "pictures" (stringToFloatDecoder |> JD.map numberToMaybe) Nothing
        |> optional "redcross" (stringToFloatDecoder |> JD.map numberToMaybe) Nothing
        |> optional "probe" (stringToFloatDecoder |> JD.map numberToMaybe) Nothing
        |> optional "border" (stringToFloatDecoder |> JD.map numberToMaybe) Nothing
        |> optional "timeout" (stringToFloatDecoder |> JD.map numberToMaybe) Nothing
        |> optional "rest" (stringToFloatDecoder |> JD.map numberToMaybe) Nothing
        |> optional "interval" (stringToFloatDecoder |> JD.map numberToMaybe) Nothing
        |> optional "width" (JD.int |> JD.map numberToMaybe) Nothing
        |> optional "height" (JD.int |> JD.map numberToMaybe) Nothing
        |> optional "blue" JD.bool False
        |> optional "gray" JD.bool False
        |> optional "dash" JD.bool False
        |> optional "targetIndex" JD.int 0
        |> optional "selectedIndex" JD.int 0
        |> optional "startIndex" JD.int 0
        |> optional "ugimageIds" (JD.list JD.string) []


stringToFloatDecoder : Decoder Float
stringToFloatDecoder =
    JD.string
        |> JD.andThen
            (\str ->
                case String.toFloat str of
                    Ok float ->
                        JD.succeed float

                    Err err ->
                        JD.fail err
            )


stringToIntDecoder : Decoder Int
stringToIntDecoder =
    JD.string
        |> JD.andThen
            (\str ->
                case String.toInt str of
                    Ok int ->
                        JD.succeed int

                    Err err ->
                        JD.fail err
            )


sessionEncoder : { a | userId : String, gameId : String, start : Time.Posix, end : Maybe Time.Posix, seed : Int, jitter : Bool } -> JE.Value
sessionEncoder { userId, gameId, start, end, seed, jitter } =
    object
        [ ( "userId", userId |> JE.string )
        , ( "gameId", gameId |> JE.string )
        , ( "seed", seed |> String.fromInt |> JE.string )
        , ( "start", start |> Time.posixToMillis |> String.fromInt |> JE.string )
        , ( "end", end |> Maybe.map (Time.posixToMillis |> String.fromInt >> JE.string) |> Maybe.withDefault JE.null )
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
        , ( "fixation", cycle.fixation |> Maybe.withDefault 0 |> (Time.posixToMillis |> String.fromInt >> JE.string) )
        , ( "selection", cycle.selection |> Maybe.withDefault 0 |> (Time.posixToMillis |> String.fromInt >> JE.string) )
        , ( "pictures", cycle.pictures |> Maybe.withDefault 0 |> (Time.posixToMillis |> String.fromInt >> JE.string) )
        , ( "redcross", cycle.redcross |> Maybe.withDefault 0 |> (Time.posixToMillis |> String.fromInt >> JE.string) )
        , ( "probe", cycle.probe |> Maybe.withDefault 0 |> (Time.posixToMillis |> String.fromInt >> JE.string) )
        , ( "border", cycle.border |> Maybe.withDefault 0 |> (Time.posixToMillis |> String.fromInt >> JE.string) )
        , ( "timeout", cycle.timeout |> Maybe.withDefault 0 |> (Time.posixToMillis |> String.fromInt >> JE.string) )
        , ( "rest", cycle.rest |> Maybe.withDefault 0 |> (Time.posixToMillis |> String.fromInt >> JE.string) )
        , ( "interval", cycle.interval |> Maybe.withDefault 0 |> (Time.posixToMillis |> String.fromInt >> JE.string) )
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
