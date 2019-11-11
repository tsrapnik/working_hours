module Main exposing (..)

import Browser
import Html exposing (Html, button, div, input, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Html.Keyed exposing (node)



--todo: replace all class identifiers with elm style css.
--if input has a value output is in the format hh:mm, where mm
--can be 00 to 59 and hh can be 00 to 99. if the int cannot
--get converted to this format or has no value --:-- is returned.


minutesToString : TimeInMinutes -> String
minutesToString timeInMinutes =
    let
        invalidTime =
            "--:--"
    in
    case timeInMinutes of
        Nothing ->
            invalidTime

        Just minutes ->
            let
                minMinutes =
                    0

                maxMinutes =
                    99 * 60 + 59
            in
            if (minutes > maxMinutes) || (minutes < minMinutes) then
                invalidTime

            else
                let
                    hours =
                        minutes // 60

                    remainingMinutes =
                        remainderBy 60 minutes

                    --we have already checked if int has valid values.
                    intToTwoDigitString : Int -> String
                    intToTwoDigitString int =
                        if int < 10 then
                            "0" ++ String.fromInt int

                        else
                            String.fromInt int
                in
                intToTwoDigitString hours ++ ":" ++ intToTwoDigitString remainingMinutes



--convert format hh:mm to int in minutes. hh can be from 00 to 99 and minute from
--00 to 59. all other formats return a nothing value.


stringToMinutes : String -> TimeInMinutes
stringToMinutes string =
    let
        maybeHours =
            String.toInt (String.slice 0 2 string)

        separatorIsCorrect =
            String.slice 2 3 string == ":"

        maybeMinutes =
            String.toInt (String.slice 3 5 string)
    in
    case ( maybeHours, separatorIsCorrect, maybeMinutes ) of
        ( Just hours, True, Just minutes ) ->
            if (hours < 0) || (hours > 99) || (minutes < 0) || (minutes > 59) then
                Nothing

            else
                Just (hours * 60 + minutes)

        default ->
            Nothing


main =
    Browser.sandbox { init = init, update = update, view = view }


type alias Model =
    { days : List Day }


init : Model
init =
    { days = workWeek }


type alias TaskId =
    Int


type alias TimeInMinutes =
    Maybe Int


type Msg
    = AddTask DayName
    | RemoveTask DayName TaskId
    | SetComment DayName TaskId String
    | SetStartTime DayName TaskId String
    | SetStopTime DayName TaskId String



--apply a function to all elements of a list which satisfy a given validation function.


applyToElementsWhichSatisfy : (a -> Bool) -> (a -> a) -> List a -> List a
applyToElementsWhichSatisfy validation function list =
    let
        applyIfSatisfies : a -> a
        applyIfSatisfies element =
            if validation element then
                function element

            else
                element
    in
    List.map applyIfSatisfies list


update : Msg -> Model -> Model
update msg model =
    let
        hasSameDayName : DayName -> Day -> Bool
        hasSameDayName dayName day =
            dayName == day.dayName

        hasSameTaskId : TaskId -> Task -> Bool
        hasSameTaskId taskId task =
            taskId == task.taskId

        applyToTasksWhichSatisfy : (Day -> Bool) -> (Task -> Bool) -> (Task -> Task) -> List Day -> List Day
        applyToTasksWhichSatisfy dayCondition taskCondition function days =
            let
                tasksFunction : List Task -> List Task
                tasksFunction tasks =
                    applyToElementsWhichSatisfy taskCondition function tasks

                dayFunction : Day -> Day
                dayFunction day =
                    { day | tasks = tasksFunction day.tasks }
            in
            applyToElementsWhichSatisfy dayCondition dayFunction days
    in
    case msg of
        AddTask dayName ->
            let
                addTask : Day -> TaskId -> Day
                addTask day newId =
                    { day | tasks = emptyTask newId :: day.tasks }

                addTaskWithCorrectId : Day -> Day
                addTaskWithCorrectId day =
                    case List.head day.tasks of
                        Just previousTask ->
                            addTask day (previousTask.taskId + 1)

                        Nothing ->
                            addTask day 0
            in
            { model | days = applyToElementsWhichSatisfy (hasSameDayName dayName) addTaskWithCorrectId model.days }

        RemoveTask dayName taskId ->
            let
                removeTaskFromList : List Task -> List Task
                removeTaskFromList tasks =
                    Tuple.second (List.partition (\task -> task.taskId == taskId) tasks)

                removeTaskFromDay : Day -> Day
                removeTaskFromDay day =
                    { day | tasks = removeTaskFromList day.tasks }
            in
            { model | days = applyToElementsWhichSatisfy (hasSameDayName dayName) removeTaskFromDay model.days }

        SetComment dayName taskId comment ->
            let
                setComment : Task -> Task
                setComment task =
                    { task | comment = comment }
            in
            { model | days = applyToTasksWhichSatisfy (hasSameDayName dayName) (hasSameTaskId taskId) setComment model.days }

        SetStartTime dayName taskId startTime ->
            let
                setStartTime : Task -> Task
                setStartTime task =
                    { task | startTime = stringToMinutes startTime }
            in
            { model | days = applyToTasksWhichSatisfy (hasSameDayName dayName) (hasSameTaskId taskId) setStartTime model.days }

        SetStopTime dayName taskId stopTime ->
            let
                setStopTime : Task -> Task
                setStopTime task =
                    { task | stopTime = stringToMinutes stopTime }
            in
            { model | days = applyToTasksWhichSatisfy (hasSameDayName dayName) (hasSameTaskId taskId) setStopTime model.days }


type alias Task =
    { taskId : TaskId
    , comment : String
    , startTime : TimeInMinutes
    , stopTime : TimeInMinutes
    }


emptyTask : TaskId -> Task
emptyTask taskId =
    { taskId = taskId
    , comment = ""
    , startTime = Nothing
    , stopTime = Nothing
    }


viewTask : DayName -> Task -> Html Msg
viewTask dayName task =
    div [ class "task" ]
        [ div [ class "top_row" ]
            [ input
                [ class "comment"
                , type_ "text"
                , value task.comment
                , onInput (SetComment dayName task.taskId)
                ]
                []
            , button
                [ class "close_button"
                , onClick (RemoveTask dayName task.taskId)
                ]
                [ text "x" ]
            ]
        , div [ class "bottom_row" ]
            [ input
                [ class "start_time"
                , type_ "time"
                , value (minutesToString task.startTime)
                , onInput (SetStartTime dayName task.taskId)
                ]
                []
            , input
                [ class "stop_time"
                , type_ "time"
                , value (minutesToString task.stopTime)
                , onInput (SetStopTime dayName task.taskId)
                ]
                []
            ]
        ]


type DayName
    = Monday
    | Tuesday
    | Wednesday
    | Thursday
    | Friday


daynameToString dayName =
    case dayName of
        Monday ->
            "monday"

        Tuesday ->
            "tuesday"

        Wednesday ->
            "wednesday"

        Thursday ->
            "thursday"

        Friday ->
            "friday"


type alias Day =
    { dayName : DayName
    , tasks : List Task
    }


viewDay day =
    div [ class "day" ]
        [ text (daynameToString day.dayName)
        , div [ class "tasks" ] (List.map (viewTask day.dayName) (List.reverse day.tasks))
        , button [ onClick (AddTask day.dayName) ] [ text "add task" ]
        ]


workWeek =
    [ { dayName = Monday
      , tasks = []
      }
    , { dayName = Tuesday
      , tasks = []
      }
    , { dayName = Wednesday
      , tasks = []
      }
    , { dayName = Thursday
      , tasks = []
      }
    , { dayName = Friday
      , tasks = []
      }
    ]


view : Model -> Html Msg
view model =
    div [ class "week" ] (List.map viewDay model.days)
