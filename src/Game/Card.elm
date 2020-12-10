module Game.Card exposing
    ( Card
    , Continuation(..)
    , andThen
    , andThenRest
    , card
    , complete
    , layout
    , restart
    , step
    , unwrap
    )

import Random
import Time exposing (Time)


type Card state layout input msg
    = Card
        { logic : input -> ( Continuation state layout input msg, Cmd msg )
        , layout : Maybe layout
        }


type Continuation state layout input msg
    = Continue state (Card state layout input msg)
    | Rest state (Card state layout input msg)
    | Complete state
    | Restart
        state
        { totalBlocks : Int
        , blockDuration : Time
        , restDuration : Time
        , nextTrials : Random.Generator (List (state -> Card state layout input msg))
        }


andThen : (state -> Bool) -> (state -> state) -> input -> (state -> Card state layout input msg) -> Card state layout input msg -> Card state layout input msg
andThen isTimeout resetSegmentStart initialize f (Card card) =
    let
        newLogic input =
            case card.logic input of
                ( Complete state, cmd1 ) ->
                    if isTimeout state then
                        ( Complete state, cmd1 )

                    else
                        let
                            ( continuation, cmd2 ) =
                                step input (f (resetSegmentStart state))
                        in
                        ( continuation, Cmd.batch [ cmd1, cmd2 ] )

                ( Continue state newCard, cmd ) ->
                    ( Continue
                        state
                        (andThen isTimeout resetSegmentStart initialize f newCard)
                    , cmd
                    )

                ( Rest state newCard, cmd ) ->
                    ( Continue
                        state
                        (andThen isTimeout resetSegmentStart initialize f newCard)
                    , cmd
                    )

                ( Restart _ _, cmd ) ->
                    Debug.crash "andThen"
    in
    Card { card | logic = newLogic }


andThenRest :
    { restCard : state -> Card state layout input msg
    , restDuration : Time
    , shouldRest : state -> Bool
    , isFinish : state -> Bool
    , isInterval : Card state layout input msg -> Bool
    , resetSegmentStart : state -> state
    , resetBlockStart : Time -> state -> state
    , initialize : input
    }
    -> (state -> Card state layout input msg)
    -> Card state layout input msg
    -> Card state layout input msg
andThenRest ({ restCard, isInterval, restDuration, shouldRest, isFinish, resetSegmentStart, resetBlockStart, initialize } as args) f (Card card) =
    let
        newLogic input =
            case card.logic input of
                ( Complete state, cmd1 ) ->
                    let
                        updatedState =
                            resetSegmentStart state
                    in
                    case ( shouldRest state, isFinish state ) of
                        ( True, False ) ->
                            ( updatedState
                                |> resetBlockStart restDuration
                                |> restCard
                                |> Rest updatedState
                            , cmd1
                            )

                        ( True, True ) ->
                            ( Complete state, cmd1 )

                        ( False, _ ) ->
                            let
                                ( continuation, cmd2 ) =
                                    step input (f updatedState)
                            in
                            ( continuation, Cmd.batch [ cmd1, cmd2 ] )

                ( Rest state newCard, cmd1 ) ->
                    continuingFromRest args cmd1 newCard f state

                ( Continue state newCard, cmd ) ->
                    ( Continue
                        state
                        (andThenRest args f newCard)
                    , cmd
                    )

                ( Restart state _, cmd ) ->
                    ( Complete state, cmd )
    in
    Card { card | logic = newLogic }


continuingFromRest :
    { restCard : state -> Card state layout input msg
    , restDuration : Time
    , initialize : input
    , isFinish : state -> Bool
    , isInterval : Card state layout input msg -> Bool
    , resetBlockStart : Time -> state -> state
    , resetSegmentStart : state -> state
    , shouldRest : state -> Bool
    }
    -> Cmd msg
    -> Card state layout input msg
    -> (state -> Card state layout input msg)
    -> state
    -> ( Continuation state layout input msg, Cmd msg )
continuingFromRest args cmd newCard f state =
    let
        ( continuation, cmd2 ) =
            step args.initialize (f (args.resetSegmentStart state))

        contCard =
            continuationCard continuation
    in
    case ( contCard, Maybe.map args.isInterval contCard ) of
        ( Just card, Just True ) ->
            let
                ( nextContinuation, cmd3 ) =
                    step args.initialize (f (args.resetSegmentStart (unwrapContinuation continuation)))
            in
            ( nextContinuation
                |> continuationCard
                |> Maybe.map (\nextCard -> Continue state (andThenRest args f nextCard))
                |> Maybe.withDefault (Complete state)
            , Cmd.batch [ cmd, cmd2, cmd3 ]
            )

        _ ->
            ( Continue
                state
                (andThenRest args f newCard)
            , cmd
            )


card : Maybe layout -> (input -> ( Continuation a layout input msg, Cmd msg )) -> Card a layout input msg
card layout logic =
    Card { layout = layout, logic = logic }


complete : a -> Card a layout input msg
complete x =
    Card { logic = always ( Complete x, Cmd.none ), layout = Nothing }


restart : { totalBlocks : Int, blockDuration : Time, restDuration : Time, nextTrials : Random.Generator (List (a -> Card a layout input msg)) } -> a -> Card a layout input msg
restart args a =
    Card { logic = always ( Restart a args, Cmd.none ), layout = Nothing }


layout : Card a layout input msg -> Maybe layout
layout (Card card) =
    card.layout


step : input -> Card a layout input msg -> ( Continuation a layout input msg, Cmd msg )
step input (Card card) =
    card.logic input


unwrap : input -> Card a layout input msg -> a
unwrap initialize card =
    case card of
        Card { logic } ->
            let
                ( continuation, _ ) =
                    logic initialize
            in
            unwrapContinuation continuation


unwrapContinuation : Continuation a layout input msg -> a
unwrapContinuation continuation =
    case continuation of
        Continue a _ ->
            a

        Rest a _ ->
            a

        Complete a ->
            a

        Restart a _ ->
            a


continuationCard : Continuation a layout input msg -> Maybe (Card a layout input msg)
continuationCard continuation =
    case continuation of
        Continue _ layout ->
            Just layout

        Rest _ layout ->
            Just layout

        Complete _ ->
            Nothing

        Restart _ _ ->
            Nothing
