module Day exposing
    ( Day
    , DayHours
    , DayIndex
    , DayNotes
    , Msg
    , dailyWorktime
    , decoder
    , decoderHours
    , decoderNotes
    , encode
    , encodeHours
    , encodeNotes
    , update
    , updateWithHours
    , updateWithNotes
    , view
    )

import Array exposing (Array)
import Array.Extra2
import Chore exposing (Chore, emptyChore)
import Date exposing (Date)
import Html exposing (Html, button, div, text, textarea, time)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Json.Decode as Decode
import Json.Encode as Encode
import Time2 exposing (TimeInMinutes)


type alias ChoreIndex =
    Int


type alias DayIndex =
    Int


type alias Day =
    { chores : Array Chore
    , notes : String
    }


type Msg
    = SetDayNotes String
    | AddChore
    | ForChoreXDo ChoreIndex Chore.Msg


view : Maybe Date -> Maybe TimeInMinutes -> DayIndex -> Day -> Html Msg
view startDate requiredMinutes dayIndex day =
    let
        choreToHtml : ChoreIndex -> Chore -> Html Msg
        choreToHtml index chore =
            Chore.view chore
                |> Html.map (ForChoreXDo index)
    in
    div [ class "day" ]
        [ div [] [ text (dayIndexToString dayIndex) ]
        , case startDate of
            Just date ->
                div [] [ text <| Date.toIsoString <| Date.add Date.Days dayIndex date ]

            Nothing ->
                div [] []
        , div [ class "chores" ] (Array.toList (Array.indexedMap choreToHtml day.chores))
        , case requiredMinutes of
            Just minutes ->
                if minutes > 0 then
                    time [ class "required_minutes_red" ] [ text (Time2.minutesToString minutes) ]

                else
                    time [ class "required_minutes_green" ] [ text (Time2.minutesToString -minutes) ]

            Nothing ->
                time [ class "required_minutes_white" ] [ text Time2.invalidTimeString ]
        , button [ onClick AddChore ] [ text "add chore" ]
        , textarea
            [ class "day_notes"
            , rows 10
            , value day.notes
            , onInput SetDayNotes
            ]
            []
        ]


update : Msg -> Day -> Day
update msg day =
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
            { day | chores = Array.push (emptyChore startTime) day.chores }

        ForChoreXDo choreIndex choreMsg ->
            { day | chores = Array.Extra2.updateWithArray choreIndex (Chore.update choreMsg) day.chores }



{- Encode or decode full day. -}


encode : Day -> Encode.Value
encode day =
    Encode.object
        [ ( "chores", Encode.array Chore.encode day.chores )
        , ( "notes", Encode.string day.notes )
        ]


decoder : Decode.Decoder Day
decoder =
    Decode.map2 Day
        (Decode.field "chores" (Decode.array Chore.decoder))
        (Decode.field "notes" Decode.string)



{- Encode or decode hours only. -}


type alias DayHours =
    { chores : Array Chore
    }


encodeHours : Day -> Encode.Value
encodeHours day =
    Encode.object
        [ ( "chores", Encode.array Chore.encode day.chores )
        ]


decoderHours : Decode.Decoder DayHours
decoderHours =
    Decode.map DayHours
        (Decode.field "chores" (Decode.array Chore.decoder))


updateWithHours : Day -> DayHours -> Day
updateWithHours day dayHours =
    { day | chores = dayHours.chores }



{- Encode or decode notes only. -}


type alias DayNotes =
    String


encodeNotes : Day -> Encode.Value
encodeNotes day =
    Encode.object
        [ ( "notes", Encode.string day.notes ) ]


decoderNotes : Decode.Decoder DayNotes
decoderNotes =
    Decode.field "notes" Decode.string


updateWithNotes : Day -> DayNotes -> Day
updateWithNotes day dayNotes =
    { day | notes = dayNotes }



{- helper functions -}


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
        |> Array.map Chore.duration
        |> Array.foldl maybeAdd (Just 0)


dayIndexToString : DayIndex -> String
dayIndexToString dayIndex =
    case dayIndex of
        0 ->
            "monday"

        1 ->
            "tuesday"

        2 ->
            "wednesday"

        3 ->
            "thursday"

        4 ->
            "friday"

        _ ->
            "unknown day"
