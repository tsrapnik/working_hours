port module Main exposing (..)

import Array exposing (Array)
import Array.Extra
import Browser
import Date exposing (Date)
import File exposing (File)
import File.Download as Download
import File.Select as Select
import Html exposing (Html, button, div, input, text, textarea, time)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Json.Decode as Decode
import Json.Encode as Encode
import Task

-- TODO: implement update date, rename and reaorder buttons, rename work to hours, rename filename working hours to hours, multiple notes per day and per total
port setStorage : Encode.Value -> Cmd msg


main : Program Encode.Value Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }


type Msg
    = AddTask DayIndex
    | RemoveTask DayIndex TaskIndex
    | SetProject DayIndex TaskIndex String
    | SetComment DayIndex TaskIndex String
    | SetDayNotes DayIndex String
    | SetNotes String
    | SetStartTime DayIndex TaskIndex String
    | SetStopTime DayIndex TaskIndex String
    | SaveWork
    | LoadWork
    | LoadedWork File
    | ParsedWork String
    | ClearWork
    | ReceiveDate Date
    | SaveNotes
    | LoadNotes
    | LoadedNotes File
    | ParsedNotes String
    | UpdateDate



{- model -}


type alias Model =
    { days : Array Day
    , startDate : Maybe Date
    , notes : String
    }


type alias Day =
    { tasks : Array Task
    , notes : String
    }


type alias DayIndex =
    Int


type alias Task =
    { project : String
    , comment : String
    , startTime : Maybe TimeInMinutes
    , stopTime : Maybe TimeInMinutes
    }


type alias TaskIndex =
    Int


type alias TimeInMinutes =
    Int


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
            , Task.perform ReceiveDate Date.today
            )


emptyWeek : Array Day
emptyWeek =
    Array.repeat 5 { tasks = Array.empty, notes = "" }


emptyTask : Maybe TimeInMinutes -> Task
emptyTask startTime =
    { project = ""
    , comment = ""
    , startTime = startTime
    , stopTime = Nothing
    }



{- view -}


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
            case ( dailyWorktime day, Array.get (Array.length array - 1) array ) of
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

        dayDataToHtml : Maybe Date -> Int -> ( Maybe TimeInMinutes, Day ) -> Html Msg
        dayDataToHtml startDate dayIndex dayData =
            viewDay startDate (Tuple.first dayData) dayIndex (Tuple.second dayData)
    in
    div []
        [ div [ class "week" ]
            (Array.toList
                (Array.indexedMap (dayDataToHtml model.startDate) daysData)
            )
        , div [ class "loadSave" ]
            [ button [ class "load_tasks", onClick SaveWork ] [ text "save tasks" ]
            , button [ class "save_tasks", onClick LoadWork ] [ text "load tasks" ]
            , button [ class "clear_tasks", onClick ClearWork ] [ text "clear tasks" ]
            , button [ class "save_notes", onClick SaveNotes ] [ text "save notes" ]
            , button [ class "load_notes", onClick LoadNotes ] [ text "load notes" ]
            , button [ class "update_date", onClick UpdateDate ] [ text "update date" ]
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


viewDay : Maybe Date -> Maybe TimeInMinutes -> DayIndex -> Day -> Html Msg
viewDay startDate requiredMinutes dayIndex day =
    div [ class "day" ]
        [ div [] [ text (dayIndexToString dayIndex) ]
        , case startDate of
            Just date ->
                div [] [ text <| Date.toIsoString <| Date.add Date.Days dayIndex date ]

            Nothing ->
                div [] []
        , div [ class "tasks" ] (Array.toList (Array.indexedMap (\taskIndex task -> viewTask dayIndex taskIndex task) day.tasks))
        , case requiredMinutes of
            Just minutes ->
                if minutes > 0 then
                    time [ class "required_minutes_red" ] [ text (minutesToString minutes) ]

                else
                    time [ class "required_minutes_green" ] [ text (minutesToString -minutes) ]

            Nothing ->
                time [ class "required_minutes_white" ] [ text invalidTimeString ]
        , button [ onClick (AddTask dayIndex) ] [ text "add task" ]
        , textarea
            [ class "day_notes"
            , rows 10
            , value day.notes
            , onInput (SetDayNotes dayIndex)
            ]
            []
        ]


viewTask : DayIndex -> TaskIndex -> Task -> Html Msg
viewTask dayIndex taskIndex task =
    div [ class "task" ]
        [ div [ class "top_row" ]
            [ input
                [ class "project"
                , type_ "text"
                , value task.project
                , onInput (SetProject dayIndex taskIndex)
                ]
                []
            , input
                [ class "comment"
                , type_ "text"
                , value task.comment
                , onInput (SetComment dayIndex taskIndex)
                ]
                []
            , button
                [ class "close_button"
                , onClick (RemoveTask dayIndex taskIndex)
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
                , onInput (SetStartTime dayIndex taskIndex)
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
                , onInput (SetStopTime dayIndex taskIndex)
                ]
                []
            ]
        ]



{- update -}


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AddTask dayIndex ->
            let
                addTask : Day -> Maybe TimeInMinutes -> Day
                addTask day startTime =
                    { day | tasks = Array.push (emptyTask startTime) day.tasks }

                updateDay : Day -> Day
                updateDay day =
                    case Array.get (Array.length day.tasks - 1) day.tasks of
                        Just previousTask ->
                            addTask day previousTask.stopTime

                        Nothing ->
                            addTask day Maybe.Nothing

                newModel : Model
                newModel =
                    { model | days = Array.Extra.update dayIndex updateDay model.days }
            in
            ( newModel
            , setStorage (encode newModel)
            )

        RemoveTask dayIndex taskIndex ->
            let
                removeTask : Day -> Day
                removeTask day =
                    { day | tasks = Array.Extra.removeAt taskIndex day.tasks }

                newModel : Model
                newModel =
                    { model | days = Array.Extra.update dayIndex removeTask model.days }
            in
            ( newModel
            , setStorage (encode newModel)
            )

        SetProject dayIndex taskIndex project ->
            let
                setProject : Task -> Task
                setProject task =
                    { task | project = project }

                updateDay : Day -> Day
                updateDay day =
                    { day | tasks = Array.Extra.update taskIndex setProject day.tasks }

                newModel : Model
                newModel =
                    { model | days = Array.Extra.update dayIndex updateDay model.days }
            in
            ( newModel
            , setStorage (encode newModel)
            )

        SetComment dayIndex taskIndex comment ->
            let
                setComment : Task -> Task
                setComment task =
                    { task | comment = comment }

                updateDay : Day -> Day
                updateDay day =
                    { day | tasks = Array.Extra.update taskIndex setComment day.tasks }

                newModel : Model
                newModel =
                    { model | days = Array.Extra.update dayIndex updateDay model.days }
            in
            ( newModel
            , setStorage (encode newModel)
            )

        SetDayNotes dayIndex notes ->
            let
                updateDay : Day -> Day
                updateDay day =
                    { day | notes = notes }

                newModel : Model
                newModel =
                    { model | days = Array.Extra.update dayIndex updateDay model.days }
            in
            ( newModel
            , setStorage (encode newModel)
            )

        SetNotes notes ->
            let
                newModel : Model
                newModel =
                    { model | notes = notes }
            in
            ( newModel
            , setStorage (encode newModel)
            )

        SetStartTime dayIndex taskIndex startTime ->
            let
                setStartTime : Task -> Task
                setStartTime task =
                    { task | startTime = stringToMinutes startTime }

                updateTask : Task -> Array Task
                updateTask task =
                    adaptToLunch (setStartTime task)

                updateDay : Day -> Day
                updateDay day =
                    { day | tasks = replaceAt taskIndex updateTask day.tasks }

                newModel : Model
                newModel =
                    { model | days = Array.Extra.update dayIndex updateDay model.days }
            in
            ( newModel
            , setStorage (encode newModel)
            )

        SetStopTime dayIndex taskIndex stopTime ->
            let
                setStopTime : Task -> Task
                setStopTime task =
                    { task | stopTime = stringToMinutes stopTime }

                updateTask : Task -> Array Task
                updateTask task =
                    adaptToLunch (setStopTime task)

                updateDay : Day -> Day
                updateDay day =
                    { day | tasks = replaceAt taskIndex updateTask day.tasks }

                newModel : Model
                newModel =
                    { model | days = Array.Extra.update dayIndex updateDay model.days }
            in
            ( newModel
            , setStorage (encode newModel)
            )

        SaveWork ->
            let
                yearAndWeek =
                    case model.startDate of
                        Just date ->
                            Date.format "YYYY_w" date

                        Nothing ->
                            ""

                fileName =
                    "working_hours_" ++ yearAndWeek ++ ".json"
            in
            ( model
            , Download.string fileName "application/json" (Encode.encode 4 (encodeWork model))
            )

        LoadWork ->
            ( model
            , Select.file [ "application/json" ] LoadedWork
            )

        LoadedWork file ->
            ( model
            , Task.perform ParsedWork (File.toString file)
            )

        ParsedWork string ->
            let
                newModel =
                    case Decode.decodeString decoderWork string of
                        Ok decodedModelWork ->
                            updateModelWithWork model decodedModelWork

                        Err _ ->
                            model
            in
            ( newModel
            , setStorage (encode newModel)
            )

        ClearWork ->
            ( model
            , Task.perform ReceiveDate Date.today
            )

        ReceiveDate today ->
            let
                previousMonday =
                    Date.floor Date.Monday today

                newModel =
                    { days = emptyWeek
                    , startDate = Just previousMonday
                    , notes = ""
                    }
            in
            ( newModel
            , setStorage (encode newModel)
            )

        SaveNotes ->
            let
                yearAndWeek =
                    case model.startDate of
                        Just date ->
                            Date.format "YYYY_w" date

                        Nothing ->
                            ""

                fileName =
                    "notes_" ++ yearAndWeek ++ ".json"
            in
            ( model
            , Download.string fileName "application/json" (Encode.encode 4 (encodeNotes model))
            )

        LoadNotes ->
            ( model
            , Select.file [ "application/json" ] LoadedNotes
            )

        LoadedNotes file ->
            ( model
            , Task.perform ParsedNotes (File.toString file)
            )

        ParsedNotes string ->
            let
                newModel =
                    case Decode.decodeString decoderNotes string of
                        Ok decodedModelNotes ->
                            updateModelWithNotes model decodedModelNotes

                        Err _ ->
                            model
            in
            ( newModel
            , setStorage (encode newModel)
            )

        UpdateDate ->
            ( model
            , setStorage (encode model)
            )



{- json -}


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
            [ [ ( "days", Encode.array encodeDay model.days ) ]
            , startDate
            , [ ( "notes", Encode.string model.notes ) ]
            ]
        )


encodeDay : Day -> Encode.Value
encodeDay day =
    Encode.object
        [ ( "tasks", Encode.array encodeTask day.tasks )
        , ( "notes", Encode.string day.notes )
        ]


encodeTask : Task -> Encode.Value
encodeTask task =
    let
        startTime =
            case task.startTime of
                Just minutes ->
                    [ ( "startTime", Encode.int minutes ) ]

                Nothing ->
                    []

        stopTime =
            case task.stopTime of
                Just minutes ->
                    [ ( "stopTime", Encode.int minutes ) ]

                Nothing ->
                    []
    in
    Encode.object
        (List.concat
            [ [ ( "project", Encode.string task.project )
              , ( "comment", Encode.string task.comment )
              ]
            , startTime
            , stopTime
            ]
        )


decoder : Decode.Decoder Model
decoder =
    Decode.map3 Model
        (Decode.field "days" (Decode.array dayDecoder))
        (Decode.maybe <| Decode.map Date.fromRataDie <| Decode.field "startDate" Decode.int)
        (Decode.field "notes" Decode.string)


dayDecoder : Decode.Decoder Day
dayDecoder =
    Decode.map2 Day
        (Decode.field "tasks" (Decode.array taskDecoder))
        (Decode.field "notes" Decode.string)


taskDecoder : Decode.Decoder Task
taskDecoder =
    Decode.map4 Task
        (Decode.field "project" Decode.string)
        (Decode.field "comment" Decode.string)
        (Decode.maybe (Decode.field "startTime" Decode.int))
        (Decode.maybe (Decode.field "stopTime" Decode.int))



{- conversions -}


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



{- calculations -}


taskTime : Task -> Maybe TimeInMinutes
taskTime task =
    case ( task.startTime, task.stopTime ) of
        ( Just startTime, Just stopTime ) ->
            if (stopTime - startTime) < 0 then
                Nothing

            else
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
        |> Array.map taskTime
        |> Array.foldl maybeAdd (Just 0)


{-| take an array and replace element at given index with zero or more elements defined by a
replacement function that takes that element as input.
-}
replaceAt : Int -> (a -> Array a) -> Array a -> Array a
replaceAt index replacement array =
    let
        left =
            Array.slice 0 index array

        right =
            Array.slice (index + 1) (Array.length array) array

        maybeElement =
            Array.get index array
    in
    case maybeElement of
        Just element ->
            Array.append left (Array.append (replacement element) right)

        Nothing ->
            array


adaptToLunch : Task -> Array Task
adaptToLunch task =
    let
        startLunch =
            12 * 60 + 30

        endLunch =
            13 * 60
    in
    case ( task.startTime, task.stopTime ) of
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
                Array.fromList [ { task | stopTime = Just startLunch } ]

            else if startsInLunch then
                --crop start time.
                Array.fromList [ { task | startTime = Just endLunch } ]

            else if inLunch then
                --remove task.
                Array.empty

            else if envelopsLunch then
                --split task in part before and after lunch.
                Array.fromList [ { task | stopTime = Just startLunch }, { task | startTime = Just endLunch } ]

            else
                --other cases we do not need to change anything.
                Array.fromList [ task ]

        _ ->
            Array.fromList [ task ]



{- partial encoders and decoders -}


encodeNotes : Model -> Encode.Value
encodeNotes model =
    Encode.object
        [ ( "days", Encode.array encodeDayNotes model.days )
        , ( "notes", Encode.string model.notes )
        ]


encodeDayNotes : Day -> Encode.Value
encodeDayNotes day =
    Encode.object
        [ ( "notes", Encode.string day.notes ) ]


encodeWork : Model -> Encode.Value
encodeWork model =
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
            [ [ ( "days", Encode.array encodeWorkDay model.days ) ]
            , startDate
            ]
        )


encodeWorkDay : Day -> Encode.Value
encodeWorkDay day =
    Encode.object
        [ ( "tasks", Encode.array encodeTask day.tasks )
        ]


type alias ModelNotes =
    { days : Array DayNotes
    , notes : String
    }


type alias DayNotes =
    String


updateModelWithNotes : Model -> ModelNotes -> Model
updateModelWithNotes model modelNotes =
    let
        updateDayWithNotes : Day -> DayNotes -> Day
        updateDayWithNotes day dayNotes =
            { day | notes = dayNotes }
    in
    { model | notes = modelNotes.notes, days = Array.Extra.map2 updateDayWithNotes model.days modelNotes.days }


decoderNotes : Decode.Decoder ModelNotes
decoderNotes =
    Decode.map2 ModelNotes
        (Decode.field "days" (Decode.array dayDecoderNotes))
        (Decode.field "notes" Decode.string)


dayDecoderNotes : Decode.Decoder DayNotes
dayDecoderNotes =
    Decode.field "notes" Decode.string


type alias ModelWork =
    { days : Array DayWork
    , startDate : Maybe Date
    }


type alias DayWork =
    { tasks : Array Task
    }


updateModelWithWork : Model -> ModelWork -> Model
updateModelWithWork model modelWork =
    let
        updateDayWithWork : Day -> DayWork -> Day
        updateDayWithWork day dayWork =
            { day | tasks = dayWork.tasks }
    in
    { model | days = Array.Extra.map2 updateDayWithWork model.days modelWork.days, startDate = modelWork.startDate }


decoderWork : Decode.Decoder ModelWork
decoderWork =
    Decode.map2 ModelWork
        (Decode.field "days" (Decode.array dayDecoderWork))
        (Decode.maybe <| Decode.map Date.fromRataDie <| Decode.field "startDate" Decode.int)


dayDecoderWork : Decode.Decoder DayWork
dayDecoderWork =
    Decode.map DayWork
        (Decode.field "tasks" (Decode.array taskDecoder))
