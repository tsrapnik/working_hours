module Main exposing (..)

import Browser
import Html exposing (Html, button, div, text, input)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Html.Keyed exposing (node)

--todo: replace all class identifiers with elm style css.

minutesToString timeInMinutes =
  case timeInMinutes of
    Nothing ->
        "--:--"

    Just minutes ->
      let
          absoluteMinutes = abs minutes
          maxMinutes = 99 * 60 + 59
      in
      if absoluteMinutes <= maxMinutes then
        let
            hours = absoluteMinutes // 60
            remainingMinutes = remainderBy 60 absoluteMinutes
        in
          (String.fromInt hours) ++ ":" ++ (String.fromInt remainingMinutes)
      else
        "--:--"

stringToMinutes string =
  Just  671

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
      applyToTaskWithSameTaskId identity taskId

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
    , div [class "tasks"] (List.map viewTask day.tasks)
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