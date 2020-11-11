port module Main exposing (..)

import Array exposing (Array)
import Array.Extra
import Browser exposing (element)
import Html exposing (Html, button, div, input, text, time)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Html.Keyed exposing (node)
import Json.Decode as D exposing (Decoder, field, int, maybe, string)
import Json.Encode as E
import List.Extra exposing (splitWhen, uncons, updateIf)


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
        |> Array.map taskTime
        |> Array.foldl maybeAdd (Just 0)


type alias TaskIndex =
    Int


type alias Task =
    { project : String
    , comment : String
    , startTime : Maybe TimeInMinutes
    , stopTime : Maybe TimeInMinutes
    }


emptyTask : Maybe TimeInMinutes -> Task
emptyTask startTime =
    { project = ""
    , comment = ""
    , startTime = startTime
    , stopTime = Nothing
    }


viewTask : DayName -> TaskIndex -> Task -> Html Msg
viewTask dayName taskIndex task =
    div [ class "task" ]
        [ div [ class "top_row" ]
            [ input
                [ class "project"
                , type_ "text"
                , value task.project
                , onInput (SetProject dayName taskIndex)
                ]
                []
            , input
                [ class "comment"
                , type_ "text"
                , value task.comment
                , onInput (SetComment dayName taskIndex)
                ]
                []
            , button
                [ class "close_button"
                , onClick (RemoveTask dayName taskIndex)
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
                , onInput (SetStartTime dayName taskIndex)
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
                , onInput (SetStopTime dayName taskIndex)
                ]
                []
            ]
        ]


type alias Day =
    { dayName : DayName
    , tasks : Array Task
    }


type DayName
    = Monday
    | Tuesday
    | Wednesday
    | Thursday
    | Friday


daynameToString : DayName -> String
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


stringToDayname : String -> DayName
stringToDayname string =
    case string of
        "monday" ->
            Monday

        "tuesday" ->
            Tuesday

        "wednesday" ->
            Wednesday

        "thursday" ->
            Thursday

        "friday" ->
            Friday

        _ ->
            Monday



--default to monday.


viewDay : Maybe TimeInMinutes -> Day -> Html Msg
viewDay requiredMinutes day =
    div [ class "day" ]
        [ text (daynameToString day.dayName)
        , div [ class "tasks" ] (Array.toList (Array.indexedMap (\taskIndex task -> viewTask day.dayName taskIndex task) day.tasks))
        , case requiredMinutes of
            Just minutes ->
                if minutes > 0 then
                    time [ class "required_minutes_red" ] [ text (minutesToString minutes) ]

                else
                    time [ class "required_minutes_green" ] [ text (minutesToString -minutes) ]

            Nothing ->
                time [ class "required_minutes_white" ] [ text invalidTimeString ]
        , button [ onClick (AddTask day.dayName) ] [ text "add task" ]
        ]


workWeek : List Day
workWeek =
    [ { dayName = Monday
      , tasks = Array.empty
      }
    , { dayName = Tuesday
      , tasks = Array.empty
      }
    , { dayName = Wednesday
      , tasks = Array.empty
      }
    , { dayName = Thursday
      , tasks = Array.empty
      }
    , { dayName = Friday
      , tasks = Array.empty
      }
    ]


main : Program E.Value Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = updateWithStorage
        , subscriptions = \_ -> Sub.none
        }


type alias Model =
    { days : List Day }


init : E.Value -> ( Model, Cmd Msg )
init flags =
    ( case D.decodeValue decoder flags of
        Ok model ->
            model

        Err _ ->
            { days = workWeek }
    , Cmd.none
    )


view : Model -> Html Msg
view model =
    let
        requiredDailyWorkTime =
            8 * 60

        addRequiredMinutesToList : Day -> List (Maybe TimeInMinutes) -> List (Maybe TimeInMinutes)
        addRequiredMinutesToList day list =
            if Array.isEmpty day.tasks then
                case List.head list of
                    Nothing ->
                        Just 0 :: list

                    Just accumulatedTime ->
                        accumulatedTime :: list

            else
                case ( dailyWorktime day, List.head list ) of
                    ( Just workTime, Nothing ) ->
                        Just (requiredDailyWorkTime - workTime) :: list

                    ( Just workTime, Just (Just accumulatedTime) ) ->
                        Just ((requiredDailyWorkTime - workTime) + accumulatedTime) :: list

                    _ ->
                        Nothing :: list

        requiredMinutes : List (Maybe TimeInMinutes)
        requiredMinutes =
            List.reverse (List.foldl addRequiredMinutesToList [] model.days)
    in
    div [ class "week" ] (List.map2 viewDay requiredMinutes model.days)


type Msg
    = AddTask DayName
    | RemoveTask DayName TaskIndex
    | SetProject DayName TaskIndex String
    | SetComment DayName TaskIndex String
    | SetStartTime DayName TaskIndex String
    | SetStopTime DayName TaskIndex String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        hasSameDayName : DayName -> Day -> Bool
        hasSameDayName dayName day =
            dayName == day.dayName
    in
    case msg of
        AddTask dayName ->
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
            in
            ( { model | days = List.Extra.updateIf (hasSameDayName dayName) updateDay model.days }
            , Cmd.none
            )

        RemoveTask dayName taskIndex ->
            let
                removeTask : Day -> Day
                removeTask day =
                    { day | tasks = Array.Extra.removeAt taskIndex day.tasks }
            in
            ( { model | days = List.Extra.updateIf (hasSameDayName dayName) removeTask model.days }
            , Cmd.none
            )

        SetProject dayName taskIndex project ->
            let
                setProject : Task -> Task
                setProject task =
                    { task | project = project }

                updateDay : Day -> Day
                updateDay day =
                    { day | tasks = Array.Extra.update taskIndex setProject day.tasks }
            in
            ( { model | days = List.Extra.updateIf (hasSameDayName dayName) updateDay model.days }
            , Cmd.none
            )

        SetComment dayName taskIndex comment ->
            let
                setComment : Task -> Task
                setComment task =
                    { task | comment = comment }

                updateDay : Day -> Day
                updateDay day =
                    { day | tasks = Array.Extra.update taskIndex setComment day.tasks }
            in
            ( { model | days = List.Extra.updateIf (hasSameDayName dayName) updateDay model.days }
            , Cmd.none
            )

        SetStartTime dayName taskIndex startTime ->
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
            in
            ( { model | days = List.Extra.updateIf (hasSameDayName dayName) updateDay model.days }
            , Cmd.none
            )

        SetStopTime dayName taskIndex stopTime ->
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
            in
            ( { model | days = List.Extra.updateIf (hasSameDayName dayName) updateDay model.days }
            , Cmd.none
            )


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
                    (startTime > startLunch) && (startTime <= endLunch)

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


port setStorage : E.Value -> Cmd msg


updateWithStorage : Msg -> Model -> ( Model, Cmd Msg )
updateWithStorage msg oldModel =
    let
        ( newModel, cmds ) =
            update msg oldModel
    in
    ( newModel
    , Cmd.batch [ setStorage (encode newModel), cmds ]
    )


encode : Model -> E.Value
encode model =
    E.object
        [ ( "days", E.list encodeDay model.days ) ]


encodeDay : Day -> E.Value
encodeDay day =
    E.object
        [ ( "dayName", E.string (daynameToString day.dayName) )
        , ( "tasks", E.array encodeTask day.tasks )
        ]


encodeTask : Task -> E.Value
encodeTask task =
    let
        startTime =
            case task.startTime of
                Just minutes ->
                    [ ( "startTime", E.int minutes ) ]

                Nothing ->
                    []

        stopTime =
            case task.stopTime of
                Just minutes ->
                    [ ( "stopTime", E.int minutes ) ]

                Nothing ->
                    []
    in
    E.object
        (List.concat
            [ [ ( "project", E.string task.project )
              , ( "comment", E.string task.comment )
              ]
            , startTime
            , stopTime
            ]
        )


decoder : D.Decoder Model
decoder =
    D.map Model
        (field "days" (D.list dayDecoder))


dayDecoder : D.Decoder Day
dayDecoder =
    D.map2 Day
        (D.map stringToDayname (field "dayName" string))
        (field "tasks" (D.array taskDecoder))


taskDecoder : D.Decoder Task
taskDecoder =
    D.map4 Task
        (field "project" string)
        (field "comment" string)
        (maybe (field "startTime" int))
        (maybe (field "stopTime" int))
