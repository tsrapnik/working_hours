port module Main exposing (main)

import Array exposing (Array)
import Array.Extra
import Browser
import Date exposing (Date)
import Day exposing (Day, DayHours, DayIndex, DayMsg, DayNotes)
import File exposing (File)
import File.Download as Download
import File.Select as Select
import Html exposing (Html, button, div, text, textarea)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Json.Decode as Decode
import Json.Encode as Encode
import Task
import Time2 exposing (TimeInMinutes)


port setStorage : Encode.Value -> Cmd msg


main : Program Encode.Value Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }



{- model -}


type alias Model =
    { days : Array Day
    , startDate : Maybe Date
    , notes : String
    }


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


emptyWeek : Array Day
emptyWeek =
    Array.repeat 5 { chores = Array.empty, notes = "" }


init : Encode.Value -> ( Model, Cmd Msg )
init flags =
    case Decode.decodeValue decoder flags of
        Ok model ->
            ( model
            , Cmd.none
            )

        Err _ ->
            ( { days = emptyWeek
              , startDate = Nothing
              , notes = ""
              }
            , Task.perform (\date -> GetDateAnd ClearHours (UseDateStep date)) Date.today
            )


view : Model -> Html Msg
view model =
    let
        requiredDailyWorkTime =
            8 * 60

        requiredWorkTime passedWorkTime =
            if passedWorkTime == 0 then
                0

            else
                requiredDailyWorkTime - passedWorkTime

        addRequiredMinutesToArray : Day -> Array (Maybe TimeInMinutes) -> Array (Maybe TimeInMinutes)
        addRequiredMinutesToArray day array =
            case ( Day.dailyWorktime day, Array.get (Array.length array - 1) array ) of
                ( Just workTime, Nothing ) ->
                    Array.push (Just (requiredWorkTime workTime)) array

                ( Just workTime, Just (Just accumulatedTime) ) ->
                    Array.push (Just (requiredWorkTime workTime + accumulatedTime)) array

                _ ->
                    Array.push Nothing array

        requiredMinutes : Array (Maybe TimeInMinutes)
        requiredMinutes =
            Array.foldl addRequiredMinutesToArray Array.empty model.days

        daysData : Array ( Maybe TimeInMinutes, Day )
        daysData =
            Array.Extra.zip requiredMinutes model.days

        dayToHtml : Maybe Date -> Int -> ( Maybe TimeInMinutes, Day ) -> Html Msg
        dayToHtml startDate dayIndex dayData =
            Day.viewDay startDate (Tuple.first dayData) dayIndex (Tuple.second dayData)
                |> Html.map (ForDayXDo dayIndex)
    in
    div []
        [ div [ class "week" ]
            (Array.toList
                (Array.indexedMap (dayToHtml model.startDate) daysData)
            )
        , div [ class "loadSave" ]
            [ button [ class "load_hours", onClick (Load Hours LoadStep) ] [ text "load hours" ]
            , button [ class "save_hours", onClick (Save Hours) ] [ text "save hours" ]
            , button [ class "clear_hours", onClick (GetDateAnd ClearHours GetDateStep) ] [ text "clear hours" ]
            , button [ class "load_notes", onClick (Load Notes LoadStep) ] [ text "load notes" ]
            , button [ class "save_notes", onClick (Save Notes) ] [ text "save notes" ]
            , button [ class "update_date", onClick (GetDateAnd UpdateDate GetDateStep) ] [ text "update date" ]
            ]
        , div []
            [ textarea
                [ class "notes"
                , rows 10
                , value model.notes
                , onInput SetNotes
                ]
                []
            ]
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        updateAndSave newModel =
            ( newModel
            , setStorage (encode newModel)
            )
    in
    case msg of
        ForDayXDo dayIndex dayMsg ->
            let
                newModel : Model
                newModel =
                    { model | days = Array.Extra.update dayIndex (Day.updateDay dayMsg) model.days }
            in
            updateAndSave newModel

        SetNotes notes ->
            let
                newModel : Model
                newModel =
                    { model | notes = notes }
            in
            updateAndSave newModel

        Save hoursOrNotes ->
            let
                yearAndWeek =
                    case model.startDate of
                        Just date ->
                            Date.format "YYYY_w" date

                        Nothing ->
                            ""

                fileNamePrefix =
                    case hoursOrNotes of
                        Hours ->
                            "hours_"

                        Notes ->
                            "notes_"

                fileName =
                    fileNamePrefix ++ yearAndWeek ++ ".json"

                localEncoder =
                    case hoursOrNotes of
                        Hours ->
                            encodeHours

                        Notes ->
                            encodeNotes
            in
            ( model
            , Download.string fileName "application/json" (Encode.encode 4 (localEncoder model))
            )

        Load hoursOrNotes step ->
            case step of
                LoadStep ->
                    ( model
                    , Select.file [ "application/json" ] (\file -> Load hoursOrNotes (LoadedStep file))
                    )

                LoadedStep file ->
                    ( model
                    , Task.perform (\string -> Load hoursOrNotes (ParsedStep string)) (File.toString file)
                    )

                ParsedStep string ->
                    let
                        decodeAndUpdate localDecoder updater =
                            case Decode.decodeString localDecoder string of
                                Ok decodedModel ->
                                    updater model decodedModel

                                Err _ ->
                                    model

                        newModel =
                            case hoursOrNotes of
                                Hours ->
                                    decodeAndUpdate decoderHours updateModelWithHours

                                Notes ->
                                    decodeAndUpdate decoderNotes updateModelWithNotes
                    in
                    updateAndSave newModel

        GetDateAnd dateMsg dateMsgStep ->
            case dateMsgStep of
                GetDateStep ->
                    ( model
                    , Task.perform (\date -> GetDateAnd dateMsg (UseDateStep date)) Date.today
                    )

                UseDateStep today ->
                    let
                        previousMonday =
                            Date.floor Date.Monday today

                        newModel =
                            case dateMsg of
                                ClearHours ->
                                    updateModelWithHours model (emptyModelHours previousMonday)

                                UpdateDate ->
                                    { model | startDate = Just previousMonday }
                    in
                    updateAndSave newModel



{- Encode or decode full model. -}


encode : Model -> Encode.Value
encode model =
    let
        startDate =
            case model.startDate of
                Just date ->
                    [ ( "startDate", Encode.int (Date.toRataDie date) ) ]

                Nothing ->
                    []
    in
    Encode.object
        (List.concat
            [ [ ( "days", Encode.array Day.encodeDay model.days ) ]
            , startDate
            , [ ( "notes", Encode.string model.notes ) ]
            ]
        )


decoder : Decode.Decoder Model
decoder =
    Decode.map3 Model
        (Decode.field "days" (Decode.array Day.dayDecoder))
        (Decode.maybe <| Decode.map Date.fromRataDie <| Decode.field "startDate" Decode.int)
        (Decode.field "notes" Decode.string)



{- Encode or decode hours only. -}


type alias ModelHours =
    { days : Array DayHours
    , startDate : Maybe Date
    }


emptyModelHours : Date -> ModelHours
emptyModelHours date =
    { days = Array.repeat 5 { chores = Array.empty }
    , startDate = Just date
    }


encodeHours : Model -> Encode.Value
encodeHours model =
    let
        startDate =
            case model.startDate of
                Just date ->
                    [ ( "startDate", Encode.int (Date.toRataDie date) ) ]

                Nothing ->
                    []
    in
    Encode.object
        (List.concat
            [ [ ( "days", Encode.array Day.encodeDayHours model.days ) ]
            , startDate
            ]
        )


decoderHours : Decode.Decoder ModelHours
decoderHours =
    Decode.map2 ModelHours
        (Decode.field "days" (Decode.array Day.dayDecoderHours))
        (Decode.maybe <| Decode.map Date.fromRataDie <| Decode.field "startDate" Decode.int)


updateModelWithHours : Model -> ModelHours -> Model
updateModelWithHours model modelHours =
    { model | days = Array.Extra.map2 Day.updateDayWithHours model.days modelHours.days, startDate = modelHours.startDate }



{- Encode or decode notes only. -}


type alias ModelNotes =
    { days : Array DayNotes
    , notes : String
    }


encodeNotes : Model -> Encode.Value
encodeNotes model =
    Encode.object
        [ ( "days", Encode.array Day.encodeDayNotes model.days )
        , ( "notes", Encode.string model.notes )
        ]


decoderNotes : Decode.Decoder ModelNotes
decoderNotes =
    Decode.map2 ModelNotes
        (Decode.field "days" (Decode.array Day.dayDecoderNotes))
        (Decode.field "notes" Decode.string)


updateModelWithNotes : Model -> ModelNotes -> Model
updateModelWithNotes model modelNotes =
    { model | notes = modelNotes.notes, days = Array.Extra.map2 Day.updateDayWithNotes model.days modelNotes.days }
