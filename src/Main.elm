module Main exposing (..)

import Browser
import Html exposing (Html, button, div, text, input)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Html.Keyed exposing (node)

--todo: replace all class identifiers with elm style css.

--if input has a value output is in the format hh:mm, where mm
--can be 00 to 59 and hh can be 00 to 99. if the int cannot 
--get converted to this format or has no value --:-- is returned.
minutesToString: TimeInMinutes -> String
minutesToString timeInMinutes =
  let
      invalidTime = "--:--"
  in
  
  case timeInMinutes of
    Nothing ->
        invalidTime
    Just minutes ->
      let
          minMinutes = 0
          maxMinutes = 99 * 60 + 59
      in
      if (minutes > maxMinutes) || (minutes < minMinutes) then
        invalidTime
      else
        let
          hours = minutes // 60
          remainingMinutes = remainderBy 60 minutes

          --we have already checked if int has valid values.
          intToTwoDigitString: Int -> String
          intToTwoDigitString int =
            if (int < 10) then
              "0" ++ String.fromInt int
            else
              String.fromInt int
        in
          (intToTwoDigitString hours) ++ ":" ++ (intToTwoDigitString remainingMinutes)

--convert format hh:mm to int in minutes. hh can be from 00 to 99 and minute from
--00 to 59. all other formats return a nothing value.
stringToMinutes: String -> TimeInMinutes
stringToMinutes string =
  let
      maybeHours = String.toInt (String.slice 0 2 string)
      separatorIsCorrect = (String.slice 2 3 string) == ":"
      maybeMinutes = String.toInt (String.slice 3 5 string)
  in
    case (maybeHours, separatorIsCorrect, maybeMinutes) of
        (Just hours, True, Just minutes) ->
          if (hours < 0) || (hours > 99) || (minutes < 0) || (minutes > 59) then
            Nothing
          else
            Just (hours * 60 + minutes)
        default ->
          Nothing

main =
  Browser.sandbox { init = init, update = update, view = view }

type alias Model = List(Day)

init : Model
init = workWeek

type alias Id = Int
type alias TaskId =
  { day : DayName
  , task : Id
  }

type alias TimeInMinutes = Maybe Int

type Msg
  = AddTask DayName
  | RemoveTask TaskId
  | SetComment TaskId String
  | SetStartTime TaskId String
  | SetStopTime  TaskId String


update : Msg -> Model -> Model
update msg model =
  let
    applyToDayWithSameName: (Day -> Day) -> DayName -> Model
    applyToDayWithSameName function dayName =
      let
        applyIfDayWithSameName: Day -> Day
        applyIfDayWithSameName day =
          if day.dayName == dayName then
            function day
          else
            day
      in
      List.map applyIfDayWithSameName model

    applyToTaskWithSameTaskId: (Task -> Task) -> TaskId -> Model
    applyToTaskWithSameTaskId function taskId =
      let
        applyToTaskWithSameId: Day -> Day
        applyToTaskWithSameId day =
          let
            applyIfTaskWithSameId: Task -> Task
            applyIfTaskWithSameId task =
              if task.taskId.task == taskId.task then
                function task
              else
                task
          in
            {day | tasks = (List.map applyIfTaskWithSameId day.tasks) }
      in
      applyToDayWithSameName applyToTaskWithSameId taskId.day
  in

  case msg of
    AddTask dayName ->
      let
        addTask: Day -> Id -> Day
        addTask day newId =
          { day | tasks = ((emptyTask day.dayName newId) :: day.tasks) }

        addTaskWithCorrectId: Day -> Day
        addTaskWithCorrectId day =
          case (List.head day.tasks) of
            Just previousTask ->
              addTask day (previousTask.taskId.task + 1)
            Nothing ->
              addTask day 0
      in
      applyToDayWithSameName addTaskWithCorrectId dayName

    RemoveTask taskId ->
      let
        removeTaskFromList: List(Task) -> List(Task)
        removeTaskFromList tasks =
          Tuple.second (List.partition (\task -> task.taskId.task == taskId.task) tasks)
        removeTaskFromDay: Day -> Day
        removeTaskFromDay day =
          {day | tasks = removeTaskFromList day.tasks}
      in
      applyToDayWithSameName removeTaskFromDay taskId.day

    SetComment taskId comment ->
      let
        setComment: Task -> Task
        setComment task =
          {task | comment = comment}
      in
      applyToTaskWithSameTaskId setComment taskId

    SetStartTime taskId startTime ->
      let
        setComment: Task -> Task
        setComment task =
          {task | startTime = stringToMinutes startTime}
      in
      applyToTaskWithSameTaskId setComment taskId

    SetStopTime taskId stopTime ->
      let
        setComment: Task -> Task
        setComment task =
          {task | stopTime = stringToMinutes stopTime}
      in
      applyToTaskWithSameTaskId setComment taskId

type alias Task =
  { taskId : TaskId
  , comment : String
  , startTime : TimeInMinutes
  , stopTime : TimeInMinutes
  }

emptyTask: DayName -> Id -> Task
emptyTask day id =
  { taskId =
      { day = day
      , task = id
      }
  , comment = ""
  , startTime = Nothing
  , stopTime = Nothing
  }

viewTask : Task -> Html Msg
viewTask task =
  div [class "task"]
    [ div[class "top_row"]
        [ input [ class "comment"
                , type_ "text"
                , value task.comment
                , onInput (SetComment task.taskId)
                ]
                []
        , button [ class "close_button"
                 , onClick (RemoveTask task.taskId)
                 ]
                 [text "x"]
        ]
    , div[class "bottom_row"]
        [ input [ class "start_time"
                , type_ "time"
                , value (minutesToString task.startTime)
                , onInput (SetStartTime task.taskId)
                ]
                []
        , input [ class "stop_time"
                , type_ "time"
                , value (minutesToString task.stopTime)
                , onInput (SetStopTime task.taskId)
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
  , tasks : List(Task)
  }

viewDay day =
  div [class "day"]
    [ text (daynameToString day.dayName)
    , div [class "tasks"] (List.map viewTask (List.reverse day.tasks)) 
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
  div [class "week"] (List.map viewDay model)