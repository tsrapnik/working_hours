module Day exposing (DateMsg(..), DateMsgStep(..), Day, DayHours, DayIndex, DayMsg, DayNotes, HoursOrNotes(..), LoadMsgStep(..), Msg(..), dailyWorktime, dayDecoder, dayDecoderHours, dayDecoderNotes, encodeDay, encodeDayHours, encodeDayNotes, updateDay, updateDayWithHours, updateDayWithNotes, viewDay)

import Array exposing (Array)
import Array.Extra2
import Common exposing (TimeInMinutes)
import Date exposing (Date)
import File exposing (File)
import Html exposing (Html, button, div, input, text, textarea, time)
import Html.Attributes exposing (..)
import Html.Events exposing (keyCode, on, onClick, onInput)
import Json.Decode as Decode
import Json.Encode as Encode


type alias Day =
    { tasks : Array Task
    , notes : String
    }


type alias DayIndex =
    Int


type alias Task =
    { project : String
    , comment : String
    , startTime : Maybe TimeInMinutes
    , stopTime : Maybe TimeInMinutes
    }


type alias TaskIndex =
    Int


type DayMsg
    = SetDayNotes String
    | AddTask
    | ForTaskXDo TaskIndex TaskMsg


type TaskMsg
    = RemoveTask
    | SetProject String
    | SetComment String
    | SetStartTime String
    | SetStopTime String
    | KeyDownStartTime Int
    | KeyDownStopTime Int



-- TODO: Move Msg, HoursOrNotes, LoadMsgStep, DateMsgStep and DateMsg.


type Msg
    = ForDayXDo DayIndex DayMsg
    | SetNotes String
    | Save HoursOrNotes
    | Load HoursOrNotes LoadMsgStep
    | GetDateAnd DateMsg DateMsgStep


type HoursOrNotes
    = Hours
    | Notes


type DateMsg
    = ClearHours
    | UpdateDate


type DateMsgStep
    = GetDateStep
    | UseDateStep Date


type LoadMsgStep
    = LoadStep
    | LoadedStep File
    | ParsedStep String


emptyTask : Maybe TimeInMinutes -> Task
emptyTask startTime =
    { project = ""
    , comment = ""
    , startTime = startTime
    , stopTime = Nothing
    }


viewDay : Maybe Date -> Maybe TimeInMinutes -> DayIndex -> Day -> Html Msg
viewDay startDate requiredMinutes dayIndex day =
    div [ class "day" ]
        [ div [] [ text (Common.intToDay dayIndex) ]
        , case startDate of
            Just date ->
                div [] [ text <| Date.toIsoString <| Date.add Date.Days dayIndex date ]

            Nothing ->
                div [] []
        , div [ class "tasks" ] (Array.toList (Array.indexedMap (\taskIndex task -> viewTask dayIndex taskIndex task) day.tasks))
        , case requiredMinutes of
            Just minutes ->
                if minutes > 0 then
                    time [ class "required_minutes_red" ] [ text (Common.minutesToString minutes) ]

                else
                    time [ class "required_minutes_green" ] [ text (Common.minutesToString -minutes) ]

            Nothing ->
                time [ class "required_minutes_white" ] [ text Common.invalidTimeString ]
        , button [ onClick (ForDayXDo dayIndex AddTask) ] [ text "add task" ]
        , textarea
            [ class "day_notes"
            , rows 10
            , value day.notes
            , onInput (\notes -> ForDayXDo dayIndex (SetDayNotes notes))
            ]
            []
        ]


onKey : (Int -> msg) -> Html.Attribute msg
onKey tagger =
    on "keyup" (Decode.map tagger keyCode)


viewTask : DayIndex -> TaskIndex -> Task -> Html Msg
viewTask dayIndex taskIndex task =
    let
        startTimeValue =
            case task.startTime of
                Just time ->
                    [ value (Common.minutesToString time) ]

                Nothing ->
                    []

        stopTimeValue =
            case task.stopTime of
                Just time ->
                    [ value (Common.minutesToString time) ]

                Nothing ->
                    []
    in
    div [ class "task" ]
        [ div [ class "top_row" ]
            [ input
                [ class "project"
                , type_ "text"
                , value task.project
                , onInput (\project -> ForDayXDo dayIndex (ForTaskXDo taskIndex (SetProject project)))
                ]
                []
            , input
                [ class "comment"
                , type_ "text"
                , value task.comment
                , onInput (\comment -> ForDayXDo dayIndex (ForTaskXDo taskIndex (SetComment comment)))
                ]
                []
            , button
                [ class "close_button"
                , onClick (ForDayXDo dayIndex (ForTaskXDo taskIndex RemoveTask))
                ]
                [ text "x" ]
            ]
        , div [ class "bottom_row" ]
            [ input
                ([ class "start_time"
                 , type_ "time"
                 , onKey (\key -> ForDayXDo dayIndex (ForTaskXDo taskIndex (KeyDownStartTime key)))
                 , onInput (\startTime -> ForDayXDo dayIndex (ForTaskXDo taskIndex (SetStartTime startTime)))
                 ]
                    ++ startTimeValue
                )
                []
            , input
                ([ class "stop_time"
                 , type_ "time"
                 , onKey (\key -> ForDayXDo dayIndex (ForTaskXDo taskIndex (KeyDownStopTime key)))
                 , onInput (\stopTime -> ForDayXDo dayIndex (ForTaskXDo taskIndex (SetStopTime stopTime)))
                 ]
                    ++ stopTimeValue
                )
                []
            ]
        ]


{-| Returns a list of tasks in stead of just one, since certain commands return two tasks (if a task was split) or none (if a task was removed).
-}
updateTask : TaskMsg -> Task -> Array Task
updateTask msg task =
    case msg of
        RemoveTask ->
            Array.empty

        SetProject project ->
            Array.fromList [ { task | project = project } ]

        SetComment comment ->
            Array.fromList [ { task | comment = comment } ]

        SetStartTime startTime ->
            Array.fromList [ { task | startTime = Common.stringToMinutes startTime } ]

        SetStopTime stopTime ->
            Array.fromList [ { task | stopTime = Common.stringToMinutes stopTime } ]

        KeyDownStartTime key ->
            if key == 13 then
                adaptToLunch task

            else
                Array.fromList [ task ]

        KeyDownStopTime key ->
            if key == 13 then
                adaptToLunch task

            else
                Array.fromList [ task ]


updateDay : DayMsg -> Day -> Day
updateDay msg day =
    case msg of
        SetDayNotes notes ->
            { day | notes = notes }

        AddTask ->
            let
                startTime =
                    case Array.get (Array.length day.tasks - 1) day.tasks of
                        Just previousTask ->
                            previousTask.stopTime

                        Nothing ->
                            Maybe.Nothing
            in
            { day | tasks = Array.push (emptyTask startTime) day.tasks }

        ForTaskXDo taskIndex taskMsg ->
            { day | tasks = Array.Extra2.updateWithArray taskIndex (updateTask taskMsg) day.tasks }


updateDayWithNotes : Day -> DayNotes -> Day
updateDayWithNotes day dayNotes =
    { day | notes = dayNotes }


updateDayWithHours : Day -> DayHours -> Day
updateDayWithHours day dayHours =
    { day | tasks = dayHours.tasks }


encodeDay : Day -> Encode.Value
encodeDay day =
    Encode.object
        [ ( "tasks", Encode.array encodeTask day.tasks )
        , ( "notes", Encode.string day.notes )
        ]


encodeTask : Task -> Encode.Value
encodeTask task =
    let
        startTime =
            case task.startTime of
                Just minutes ->
                    [ ( "startTime", Encode.int minutes ) ]

                Nothing ->
                    []

        stopTime =
            case task.stopTime of
                Just minutes ->
                    [ ( "stopTime", Encode.int minutes ) ]

                Nothing ->
                    []
    in
    Encode.object
        (List.concat
            [ [ ( "project", Encode.string task.project )
              , ( "comment", Encode.string task.comment )
              ]
            , startTime
            , stopTime
            ]
        )


encodeDayNotes : Day -> Encode.Value
encodeDayNotes day =
    Encode.object
        [ ( "notes", Encode.string day.notes ) ]


encodeDayHours : Day -> Encode.Value
encodeDayHours day =
    Encode.object
        [ ( "tasks", Encode.array encodeTask day.tasks )
        ]


type alias DayNotes =
    String


dayDecoder : Decode.Decoder Day
dayDecoder =
    Decode.map2 Day
        (Decode.field "tasks" (Decode.array taskDecoder))
        (Decode.field "notes" Decode.string)


taskDecoder : Decode.Decoder Task
taskDecoder =
    Decode.map4 Task
        (Decode.field "project" Decode.string)
        (Decode.field "comment" Decode.string)
        (Decode.maybe (Decode.field "startTime" Decode.int))
        (Decode.maybe (Decode.field "stopTime" Decode.int))


dayDecoderNotes : Decode.Decoder DayNotes
dayDecoderNotes =
    Decode.field "notes" Decode.string


type alias DayHours =
    { tasks : Array Task
    }


dayDecoderHours : Decode.Decoder DayHours
dayDecoderHours =
    Decode.map DayHours
        (Decode.field "tasks" (Decode.array taskDecoder))


taskTime : Task -> Maybe TimeInMinutes
taskTime task =
    case ( task.startTime, task.stopTime ) of
        ( Just startTime, Just stopTime ) ->
            if (stopTime - startTime) < 0 then
                Nothing

            else
                Just (stopTime - startTime)

        _ ->
            Nothing


dailyWorktime : Day -> Maybe TimeInMinutes
dailyWorktime day =
    let
        maybeAdd : Maybe TimeInMinutes -> Maybe TimeInMinutes -> Maybe TimeInMinutes
        maybeAdd first second =
            case ( first, second ) of
                ( Just firstTime, Just secondTime ) ->
                    Just (firstTime + secondTime)

                _ ->
                    Nothing
    in
    day.tasks
        |> Array.map taskTime
        |> Array.foldl maybeAdd (Just 0)


adaptToLunch : Task -> Array Task
adaptToLunch task =
    let
        startLunch =
            12 * 60 + 30

        endLunch =
            13 * 60
    in
    case ( task.startTime, task.stopTime ) of
        ( Just startTime, Just stopTime ) ->
            let
                startTimeBeforeLunch =
                    startTime < startLunch

                startTimeInLunch =
                    (startTime >= startLunch) && (startTime < endLunch)

                stopTimeInLunch =
                    (stopTime > startLunch) && (stopTime <= endLunch)

                stopTimeAfterLunch =
                    stopTime > endLunch

                endsInLunch =
                    startTimeBeforeLunch && stopTimeInLunch

                inLunch =
                    startTimeInLunch && stopTimeInLunch

                startsInLunch =
                    startTimeInLunch && stopTimeAfterLunch

                envelopsLunch =
                    startTimeBeforeLunch && stopTimeAfterLunch
            in
            if endsInLunch then
                --crop stop time.
                Array.fromList [ { task | stopTime = Just startLunch } ]

            else if startsInLunch then
                --crop start time.
                Array.fromList [ { task | startTime = Just endLunch } ]

            else if inLunch then
                --remove task.
                Array.empty

            else if envelopsLunch then
                --split task in part before and after lunch.
                Array.fromList [ { task | stopTime = Just startLunch }, { task | startTime = Just endLunch } ]

            else
                --other cases we do not need to change anything.
                Array.fromList [ task ]

        _ ->
            Array.fromList [ task ]
