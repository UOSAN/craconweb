module Game.Cycle exposing (generate)

import Game
import Time


generate : String -> List Game.LogEntry -> List Game.Cycle
generate sessionId logEntries =
    logEntries
        |> List.foldr (fillCycles sessionId) []


fillCycles : String -> Game.LogEntry -> List Game.Cycle -> List Game.Cycle
fillCycles sessionId logEntry cycles =
    case logEntry of
        Game.BeginSession { seed } time ->
            cycles

        Game.EndSession time ->
            cycles

        Game.BeginTrial time ->
            beginCycle { sessionId = sessionId, time = time, sort = List.length cycles } cycles

        Game.EndTrial time ->
            cycles

        Game.BeginDisplay maybeLayout time ->
            beginDisplay { sessionId = sessionId, maybeLayout = maybeLayout, time = time } cycles

        Game.BeginInput time ->
            cycles

        Game.AcceptIndication { desired } time ->
            acceptIndication { desired = desired, time = time } cycles

        Game.AcceptDirection { desired, actual } time ->
            acceptDirection { desired = desired, actual = actual, time = time } cycles

        Game.AcceptSelection { desired, actual } time ->
            acceptSelection { desired = desired, actual = actual, time = time } cycles

        Game.Timeout { desired } time ->
            timeout time cycles


beginCycle : { sessionId : String, time : Time.Posix, sort : Int } -> List Game.Cycle -> List Game.Cycle
beginCycle { sessionId, time, sort } cycles =
    { id = Nothing
    , sessionId = sessionId
    , sort = sort
    , fixation = Nothing
    , selection = Nothing
    , pictures = Nothing
    , redcross = Nothing
    , probe = Nothing
    , border = Nothing
    , timeout = Nothing
    , rest = Nothing
    , interval = Nothing
    , width = Nothing
    , height = Nothing
    , blue = False
    , gray = False
    , dash = False
    , targetIndex = 0
    , selectedIndex = 0
    , startIndex = 0
    , images = []
    }
        :: cycles


beginDisplay : { sessionId : String, time : Time.Posix, maybeLayout : Maybe Game.Layout } -> List Game.Cycle -> List Game.Cycle
beginDisplay { sessionId, time, maybeLayout } cycles =
    case ( maybeLayout, cycles ) of
        ( _, [] ) ->
            cycles

        ( Nothing, _ ) ->
            cycles

        ( Just (Game.Info _ _), _ ) ->
            cycles

        ( Just (Game.Single borderType image), cycle :: tail ) ->
            let
                updatedCycle =
                    if cycle.pictures == Nothing then
                        { cycle
                            | pictures = Just time
                            , images = [ image.id ]
                            , startIndex = 0
                            , targetIndex = 0
                            , selectedIndex = 0
                        }

                    else
                        cycle
            in
            (updatedCycle :: tail)
                |> beginBorder
                    { borderType = borderType
                    , time = time
                    }

        ( Just (Game.LeftOrRight borderType direction image), cycle :: tail ) ->
            let
                targetIndex =
                    Game.directionToIndex direction

                updatedCycle =
                    if cycle.pictures == Nothing then
                        { cycle
                            | pictures = Just time
                            , images = [ image.id ]
                            , startIndex = targetIndex
                            , targetIndex = targetIndex
                            , selectedIndex = 0
                            , width = Just 2
                        }

                    else
                        cycle
            in
            (updatedCycle :: tail)
                |> beginBorder { borderType = borderType, time = time }

        ( Just (Game.LeftRight borderType direction leftImage rightImage), cycle :: tail ) ->
            let
                updatedCycle =
                    if cycle.pictures == Nothing then
                        { cycle
                            | pictures = Just time
                            , images = [ leftImage.id, rightImage.id ]
                            , startIndex = 0
                            , width = Just 2
                            , targetIndex = Game.directionToIndex direction
                        }

                    else
                        cycle
            in
            (updatedCycle :: tail)
                |> beginBorder { borderType = borderType, time = time }

        ( Just (Game.SelectGrid borderType { columns, images, goIndex }), cycle :: tail ) ->
            let
                updatedCycle =
                    if cycle.pictures == Nothing then
                        { cycle
                            | pictures = Just time
                            , images = List.map .id images
                            , startIndex = 0
                            , targetIndex = goIndex
                            , selectedIndex = 0
                            , width = Just columns
                            , height = Just (List.length images // columns)
                        }

                    else
                        cycle
            in
            (updatedCycle :: tail)
                |> beginBorder { borderType = borderType, time = time }

        ( Just (Game.RedCross borderType), cycle :: tail ) ->
            let
                updatedCycle =
                    if cycle.redcross == Nothing then
                        { cycle | redcross = Just time }

                    else
                        cycle
            in
            (updatedCycle :: tail)
                |> beginBorder { borderType = borderType, time = time }

        ( Just (Game.Fixation borderType), cycle :: tail ) ->
            let
                updatedCycle =
                    if cycle.fixation == Nothing then
                        { cycle | fixation = Just time }

                    else
                        cycle
            in
            (updatedCycle :: tail)
                |> beginBorder { borderType = borderType, time = time }

        ( Just (Game.Probe borderType direction), cycle :: tail ) ->
            let
                updatedCycle =
                    if cycle.probe == Nothing then
                        { cycle
                            | probe = Just time
                            , targetIndex = Game.directionToIndex direction
                        }

                    else
                        cycle
            in
            (updatedCycle :: tail)
                |> beginBorder { borderType = borderType, time = time }

        ( Just Game.Rest, cycle :: tail ) ->
            let
                updatedCycle =
                    if cycle.rest == Nothing then
                        { cycle | rest = Just time }

                    else
                        cycle
            in
            updatedCycle :: tail

        ( Just Game.Interval, cycle :: tail ) ->
            let
                updatedCycle =
                    if cycle.interval == Nothing then
                        { cycle | interval = Just time }

                    else
                        cycle
            in
            updatedCycle :: tail


beginBorder : { borderType : Game.BorderType, time : Time.Posix } -> List Game.Cycle -> List Game.Cycle
beginBorder { borderType, time } cycles =
    case ( borderType, cycles ) of
        ( _, [] ) ->
            cycles

        ( Game.None, _ ) ->
            cycles

        ( Game.Blue, cycle :: tail ) ->
            if cycle.border == Nothing then
                { cycle | border = Just time, blue = True } :: tail

            else
                cycles

        ( Game.Gray, cycle :: tail ) ->
            if cycle.border == Nothing then
                { cycle | border = Just time, gray = True } :: tail

            else
                cycles

        ( Game.Black, cycle :: tail ) ->
            if cycle.border == Nothing then
                { cycle | border = Just time, blue = False, gray = False } :: tail

            else
                cycles

        ( Game.Dashed, cycle :: tail ) ->
            if cycle.border == Nothing then
                { cycle | border = Just time, dash = True } :: tail

            else
                cycles


timeout : Time.Posix -> List Game.Cycle -> List Game.Cycle
timeout time cycles =
    case cycles of
        [] ->
            cycles

        cycle :: tail ->
            { cycle | timeout = Just time } :: tail


acceptDirection : { desired : Game.Direction, actual : Game.Direction, time : Time.Posix } -> List Game.Cycle -> List Game.Cycle
acceptDirection { desired, actual, time } cycles =
    case cycles of
        [] ->
            cycles

        cycle :: tail ->
            { cycle | selection = Just time, selectedIndex = Game.directionToIndex actual } :: tail


acceptIndication : { desired : Bool, time : Time.Posix } -> List Game.Cycle -> List Game.Cycle
acceptIndication { desired, time } cycles =
    case cycles of
        [] ->
            cycles

        cycle :: tail ->
            { cycle | selection = Just time, selectedIndex = 0 } :: tail


acceptSelection : { desired : Int, actual : Int, time : Time.Posix } -> List Game.Cycle -> List Game.Cycle
acceptSelection { desired, actual, time } cycles =
    case cycles of
        [] ->
            cycles

        cycle :: tail ->
            { cycle | selection = Just time, selectedIndex = actual } :: tail
