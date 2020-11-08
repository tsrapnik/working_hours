module Main exposing (..)

import Browser
import Html exposing (Html, button, div, input, text, time)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Html.Keyed exposing (node)
import List.Extra exposing (updateIf)



--TODO: replace all class identifiers with elm style css.


type alias TimeInMinutes =
    Int


invalidTimeString : String
invalidTimeString =
    "--:--"


{-| output is in the format hh:mm, where mm
can be 00 to 59 and hh can be 00 to 99. if the int cannot
get converted to this format --:-- is returned.
-}
minutesToString : TimeInMinutes -> String
minutesToString timeInMinutes =
    let
        minMinutes =
            0

        maxMinutes =
            99 * 60 + 59
    in
    if (timeInMinutes > maxMinutes) || (timeInMinutes < minMinutes) then
        invalidTimeString

    else
        let
            hours =
                timeInMinutes // 60

            remainingMinutes =
                remainderBy 60 timeInMinutes

            --we have already checked if int has valid values.
            intToTwoDigitString : Int -> String
            intToTwoDigitString int =
                if int < 10 then
                    "0" ++ String.fromInt int

                else
                    String.fromInt int
        in
        intToTwoDigitString hours ++ ":" ++ intToTwoDigitString remainingMinutes


{-| convert format hh:mm to int in minutes. hh can be from 00 to 99 and minute from
00 to 59. all other formats return a nothing value.
-}
stringToMinutes : String -> Maybe TimeInMinutes
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

        _ ->
            Nothing


taskTime : Task -> Maybe TimeInMinutes
taskTime task =
    case ( task.startTime, task.stopTime ) of
        ( Just startTime, Just stopTime ) ->
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
        |> List.map taskTime
        |> List.foldl maybeAdd (Just 0)


type alias TaskId =
    Int


type alias Task =
    { taskId : TaskId
    , project : String
    , comment : String
    , startTime : Maybe TimeInMinutes
    , stopTime : Maybe TimeInMinutes
    }


emptyTask : TaskId -> Task
emptyTask taskId =
    { taskId = taskId
    , project = ""
    , comment = ""
    , startTime = Nothing
    , stopTime = Nothing
    }


viewTask : DayName -> Task -> Html Msg
viewTask dayName task =
    div [ class "task" ]
        [ div [ class "top_row" ]
            [ input
                [ class "project"
                , type_ "text"
                , value task.project
                , onInput (SetProject dayName task.taskId)
                ]
                []
            , input
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
                , case task.startTime of
                    Just time ->
                        value (minutesToString time)

                    Nothing ->
                        value invalidTimeString
                , onInput (SetStartTime dayName task.taskId)
                ]
                []
            , input
                [ class "stop_time"
                , type_ "time"
                , case task.stopTime of
                    Just time ->
                        value (minutesToString time)

                    Nothing ->
                        value invalidTimeString
                , onInput (SetStopTime dayName task.taskId)
                ]
                []
            ]
        ]


type alias Day =
    { dayName : DayName
    , tasks : List Task
    }


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


viewDay : Maybe TimeInMinutes -> Day -> Html Msg
viewDay requiredMinutes day =
    div [ class "day" ]
        [ text (daynameToString day.dayName)
        , div [ class "tasks" ] (List.map (viewTask day.dayName) (List.reverse day.tasks))
        , time [ class "required_minutes" ]
            [ case requiredMinutes of
                Just minutes ->
                    text (minutesToString minutes)

                Nothing ->
                    text invalidTimeString
            ]
        , button [ onClick (AddTask day.dayName) ] [ text "add task" ]
        ]


workWeek : List Day
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


main : Program () Model Msg
main =
    Browser.sandbox { init = init, update = update, view = view }


type alias Model =
    { days : List Day }


init : Model
init =
    { days = workWeek }


view : Model -> Html Msg
view model =
    let
        addAccumulatedDailyWorktimeToList : Day -> List (Maybe TimeInMinutes) -> List (Maybe TimeInMinutes)
        addAccumulatedDailyWorktimeToList day list =
            case ( dailyWorktime day, List.head list ) of
                ( Just newTime, Nothing ) ->
                    Just newTime :: list

                ( Just newTime, Just (Just accumulatedTime) ) ->
                    Just (newTime + accumulatedTime) :: list

                _ ->
                    Nothing :: list

        requiredMinutes : List (Maybe TimeInMinutes)
        requiredMinutes =
            List.reverse (List.foldl addAccumulatedDailyWorktimeToList [] model.days)
    in
    div [ class "week" ] (List.map2 viewDay requiredMinutes model.days)


type Msg
    = AddTask DayName
    | RemoveTask DayName TaskId
    | SetProject DayName TaskId String
    | SetComment DayName TaskId String
    | SetStartTime DayName TaskId String
    | SetStopTime DayName TaskId String


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
                    List.Extra.updateIf taskCondition function tasks

                dayFunction : Day -> Day
                dayFunction day =
                    { day | tasks = tasksFunction day.tasks }
            in
            List.Extra.updateIf dayCondition dayFunction days
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
            { model | days = List.Extra.updateIf (hasSameDayName dayName) addTaskWithCorrectId model.days }

        RemoveTask dayName taskId ->
            let
                removeTaskFromList : List Task -> List Task
                removeTaskFromList tasks =
                    Tuple.second (List.partition (\task -> task.taskId == taskId) tasks)

                removeTaskFromDay : Day -> Day
                removeTaskFromDay day =
                    { day | tasks = removeTaskFromList day.tasks }
            in
            { model | days = List.Extra.updateIf (hasSameDayName dayName) removeTaskFromDay model.days }

        SetProject dayName taskId project ->
            let
                setProject : Task -> Task
                setProject task =
                    { task | project = project }
            in
            { model | days = applyToTasksWhichSatisfy (hasSameDayName dayName) (hasSameTaskId taskId) setProject model.days }

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
