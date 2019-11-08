module Main exposing (..)

import Browser
import Html exposing (Html, button, div, text, input)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Html.Keyed exposing (node)

myNode =
  node "div" 

-- MAIN


main =
  Browser.sandbox { init = init, update = update, view = view }

-- MODEL


type alias Model = List (Task)


init : Model
init = []



-- UPDATE

type alias Id = Int

type Msg
  = AddTask
  | UpdateTask Id String


update :   Msg -> Model -> Model
update msg model =
  case msg of
    AddTask ->
      case (List.head model) of
        Just task ->
          (Task (task.id + 1) "") :: model
        Nothing ->
          (Task 0 "") :: model
    UpdateTask id string ->
      let
          updateOneTask task =
            if task.id == id then
              {task | comment = string}
            else
              task
      in
        List.map updateOneTask model



-- VIEW

type alias Task = { id : Id, comment : String }

viewTask : Task -> Html Msg
viewTask task =
  div []
    [ text task.comment
    , input [ placeholder "new task", onInput (UpdateTask task.id)] []
    ]

add_task_button =
  button [ onClick AddTask ] [ text "add task" ]

view : Model -> Html Msg
view model =
  div []
  [ div [] (List.map viewTask model)
  , add_task_button
  ]