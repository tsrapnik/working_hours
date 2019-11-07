module Main exposing (..)
-- Press buttons to increment and decrement a counter.
--
-- Read how it works:
--   https://guide.elm-lang.org/architecture/buttons.html
--


import Browser
import Html exposing (Html, button, div, text, input)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)



-- MAIN


main =
  Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type alias Model = List (Html Msg)


init : Model
init = [div [] [ text "i am at the top of the list" ] ]



-- UPDATE

type Msg
  = AddTask
  | UpdateTask String


update : Msg -> Model -> Model
update msg model =
  case msg of
    AddTask ->
      List.append model [task "task"]
    UpdateTask string ->
      model



-- VIEW

task : String -> Html Msg
task task_text =
  div []
    [ text task_text
    , input [ placeholder "new task", value "", onInput UpdateTask ] []
    ]

add_task_button =
  button [ onClick AddTask ] [ text "add task" ]

view : Model -> Html Msg
view model =
  div [] (List.append model [add_task_button])