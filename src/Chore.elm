module Chore exposing (Chore, Msg(..), decoder, duration, emptyChore, encode, update, view)

import Array exposing (Array)
import Html exposing (Html, button, div, input, text, time)
import Html.Attributes exposing (..)
import Html.Events exposing (keyCode, on, onClick, onInput)
import Json.Decode as Decode
import Json.Encode as Encode
import Time2 exposing (TimeInMinutes)


type alias Chore =
    { project : String
    , comment : String
    , startTime : Maybe TimeInMinutes
    , stopTime : Maybe TimeInMinutes
    }


type Msg
    = RemoveChore
    | SetProject String
    | SetComment String
    | SetStartTime String
    | SetStopTime String
    | KeyDownStartTime Int
    | KeyDownStopTime Int


emptyChore : Maybe TimeInMinutes -> Chore
emptyChore startTime =
    { project = ""
    , comment = ""
    , startTime = startTime
    , stopTime = Nothing
    }


view : Chore -> Html Msg
view chore =
    let
        onKey : (Int -> msg) -> Html.Attribute msg
        onKey tagger =
            on "keyup" (Decode.map tagger keyCode)

        startTimeValue =
            case chore.startTime of
                Just time ->
                    [ value (Time2.minutesToString time) ]

                Nothing ->
                    []

        stopTimeValue =
            case chore.stopTime of
                Just time ->
                    [ value (Time2.minutesToString time) ]

                Nothing ->
                    []
    in
    div [ class "chore" ]
        [ div [ class "top_row" ]
            [ input
                [ class "project"
                , type_ "text"
                , value chore.project
                , onInput SetProject
                ]
                []
            , input
                [ class "comment"
                , type_ "text"
                , value chore.comment
                , onInput SetComment
                ]
                []
            , button
                [ class "close_button"
                , onClick RemoveChore
                ]
                [ text "x" ]
            ]
        , div [ class "bottom_row" ]
            [ input
                ([ class "start_time"
                 , type_ "time"
                 , onKey KeyDownStartTime
                 , onInput SetStartTime
                 ]
                    ++ startTimeValue
                )
                []
            , input
                ([ class "stop_time"
                 , type_ "time"
                 , onKey KeyDownStopTime
                 , onInput SetStopTime
                 ]
                    ++ stopTimeValue
                )
                []
            ]
        ]


{-| Returns a list of chores in stead of just one, since certain commands return two chores (if a chore was split) or none (if a chore was removed).
-}
update : Msg -> Chore -> Array Chore
update msg chore =
    case msg of
        RemoveChore ->
            Array.empty

        SetProject project ->
            Array.fromList [ { chore | project = project } ]

        SetComment comment ->
            Array.fromList [ { chore | comment = comment } ]

        SetStartTime startTime ->
            Array.fromList [ { chore | startTime = Time2.stringToMinutes startTime } ]

        SetStopTime stopTime ->
            Array.fromList [ { chore | stopTime = Time2.stringToMinutes stopTime } ]

        KeyDownStartTime key ->
            if key == 13 then
                adaptToLunch chore

            else
                Array.fromList [ chore ]

        KeyDownStopTime key ->
            if key == 13 then
                adaptToLunch chore

            else
                Array.fromList [ chore ]



{- Encode or decode full chore. -}


encode : Chore -> Encode.Value
encode chore =
    let
        startTime =
            case chore.startTime of
                Just minutes ->
                    [ ( "startTime", Encode.int minutes ) ]

                Nothing ->
                    []

        stopTime =
            case chore.stopTime of
                Just minutes ->
                    [ ( "stopTime", Encode.int minutes ) ]

                Nothing ->
                    []
    in
    Encode.object
        (List.concat
            [ [ ( "project", Encode.string chore.project )
              , ( "comment", Encode.string chore.comment )
              ]
            , startTime
            , stopTime
            ]
        )


decoder : Decode.Decoder Chore
decoder =
    Decode.map4 Chore
        (Decode.field "project" Decode.string)
        (Decode.field "comment" Decode.string)
        (Decode.maybe (Decode.field "startTime" Decode.int))
        (Decode.maybe (Decode.field "stopTime" Decode.int))



{- helper functions -}


duration : Chore -> Maybe TimeInMinutes
duration chore =
    case ( chore.startTime, chore.stopTime ) of
        ( Just startTime, Just stopTime ) ->
            if (stopTime - startTime) < 0 then
                Nothing

            else
                Just (stopTime - startTime)

        _ ->
            Nothing


adaptToLunch : Chore -> Array Chore
adaptToLunch chore =
    let
        startLunch =
            12 * 60 + 30

        endLunch =
            13 * 60
    in
    case ( chore.startTime, chore.stopTime ) of
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
                Array.fromList [ { chore | stopTime = Just startLunch } ]

            else if startsInLunch then
                --crop start time.
                Array.fromList [ { chore | startTime = Just endLunch } ]

            else if inLunch then
                --remove chore.
                Array.empty

            else if envelopsLunch then
                --split chore in part before and after lunch.
                Array.fromList [ { chore | stopTime = Just startLunch }, { chore | startTime = Just endLunch } ]

            else
                --other cases we do not need to change anything.
                Array.fromList [ chore ]

        _ ->
            Array.fromList [ chore ]
