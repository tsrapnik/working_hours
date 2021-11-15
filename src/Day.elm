module Day exposing
    ( Day
    , DayHours
    , DayNotes
    , dailyWorktime
    , dayDecoder
    , dayDecoderHours
    , dayDecoderNotes
    , encodeDay
    , encodeDayHours
    , encodeDayNotes
    , updateDay
    , updateDayWithHours
    , updateDayWithNotes
    , viewDay
    )

import Array exposing (Array)
import Array.Extra2
import Chore exposing (Chore, ChoreMsg(..), DateMsg(..), DateMsgStep(..), DayIndex, DayMsg(..), HoursOrNotes(..), LoadMsgStep(..), Msg(..))
import Common exposing (TimeInMinutes)
import Date exposing (Date)
import Html exposing (Html, button, div, text, textarea, time)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Json.Decode as Decode
import Json.Encode as Encode


type alias Day =
    { chores : Array Chore
    , notes : String
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
        , div [ class "chores" ] (Array.toList (Array.indexedMap (\choreIndex chore -> Chore.viewChore dayIndex choreIndex chore) day.chores))
        , case requiredMinutes of
            Just minutes ->
                if minutes > 0 then
                    time [ class "required_minutes_red" ] [ text (Common.minutesToString minutes) ]

                else
                    time [ class "required_minutes_green" ] [ text (Common.minutesToString -minutes) ]

            Nothing ->
                time [ class "required_minutes_white" ] [ text Common.invalidTimeString ]
        , button [ onClick (ForDayXDo dayIndex AddChore) ] [ text "add chore" ]
        , textarea
            [ class "day_notes"
            , rows 10
            , value day.notes
            , onInput (\notes -> ForDayXDo dayIndex (SetDayNotes notes))
            ]
            []
        ]


updateDay : DayMsg -> Day -> Day
updateDay msg day =
    case msg of
        SetDayNotes notes ->
            { day | notes = notes }

        AddChore ->
            let
                startTime =
                    case Array.get (Array.length day.chores - 1) day.chores of
                        Just previousChore ->
                            previousChore.stopTime

                        Nothing ->
                            Maybe.Nothing
            in
            { day | chores = Array.push (Chore.emptyChore startTime) day.chores }

        ForChoreXDo choreIndex choreMsg ->
            { day | chores = Array.Extra2.updateWithArray choreIndex (Chore.updateChore choreMsg) day.chores }


updateDayWithNotes : Day -> DayNotes -> Day
updateDayWithNotes day dayNotes =
    { day | notes = dayNotes }


updateDayWithHours : Day -> DayHours -> Day
updateDayWithHours day dayHours =
    { day | chores = dayHours.chores }


encodeDay : Day -> Encode.Value
encodeDay day =
    Encode.object
        [ ( "chores", Encode.array Chore.encodeChore day.chores )
        , ( "notes", Encode.string day.notes )
        ]


encodeDayNotes : Day -> Encode.Value
encodeDayNotes day =
    Encode.object
        [ ( "notes", Encode.string day.notes ) ]


encodeDayHours : Day -> Encode.Value
encodeDayHours day =
    Encode.object
        [ ( "chores", Encode.array Chore.encodeChore day.chores )
        ]


type alias DayNotes =
    String


dayDecoder : Decode.Decoder Day
dayDecoder =
    Decode.map2 Day
        (Decode.field "chores" (Decode.array Chore.choreDecoder))
        (Decode.field "notes" Decode.string)


dayDecoderNotes : Decode.Decoder DayNotes
dayDecoderNotes =
    Decode.field "notes" Decode.string


type alias DayHours =
    { chores : Array Chore
    }


dayDecoderHours : Decode.Decoder DayHours
dayDecoderHours =
    Decode.map DayHours
        (Decode.field "chores" (Decode.array Chore.choreDecoder))


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
    day.chores
        |> Array.map Chore.choreTime
        |> Array.foldl maybeAdd (Just 0)
