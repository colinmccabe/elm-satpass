module PassFilter exposing (Model, Msg, init, pred, update, view)

import Html as H exposing (Html)
import Html.Attributes as HA
import Html.Events
import Json.Decode as JD
import Time
import Types exposing (..)


type alias Hour =
    Int


type alias Model =
    { satName : String
    , minEl : Deg
    , afterHour : Hour
    , beforeHour : Hour
    }


init : Model
init =
    { satName = ""
    , minEl = 30
    , afterHour = 0
    , beforeHour = 23
    }


type Msg
    = SatName String
    | MinEl Deg
    | AfterHour Hour
    | BeforeHour Hour
    | Reset


update : Msg -> Model -> Model
update msg model =
    case msg of
        SatName satMay ->
            { model | satName = satMay }

        MinEl el ->
            { model | minEl = el }

        AfterHour hr ->
            { model | afterHour = hr }

        BeforeHour hr ->
            { model | beforeHour = hr }

        Reset ->
            init


view : Model -> Html Msg
view filter =
    H.div []
        [ H.div
            [ HA.class "row"
            , HA.style "margin-top" "15px"
            ]
            [ H.div [ HA.class "col-xs-6" ]
                [ slider "Start hour"
                    AfterHour
                    ( 0, 1, 23 )
                    filter.afterHour
                ]
            , H.div [ HA.class "col-xs-6" ]
                [ slider "End hour"
                    BeforeHour
                    ( 0, 1, 23 )
                    filter.beforeHour
                ]
            ]
        , H.div [ HA.class "row" ]
            [ H.div [ HA.class "col-xs-4" ]
                [ slider "Min El"
                    (toFloat >> MinEl)
                    ( 5, 5, 89 )
                    (round filter.minEl)
                ]
            , H.div [ HA.class "col-xs-4" ]
                [ satNameFilter ]
            , H.div [ HA.class "col-xs-4" ]
                [ H.label [] []
                , H.button
                    [ HA.class "btn btn-primary"
                    , HA.type_ "submit"
                    , HA.style "display" "block"
                    , HA.style "width" "100%"
                    , Html.Events.onClick Reset
                    ]
                    [ H.text "Reset" ]
                ]
            ]
        ]


slider : String -> (Int -> a) -> ( Int, Int, Int ) -> Int -> Html a
slider title action ( min, step, max ) currentVal =
    let
        decodeEvent =
            Html.Events.targetValue
                |> JD.map String.toInt
                |> JD.andThen
                    (\maybeInt ->
                        case maybeInt of
                            Just i ->
                                JD.succeed i

                            Nothing ->
                                JD.fail "Failed to convert slider target to int"
                    )
                |> JD.map action
    in
    H.div [ HA.class "form-group" ]
        [ H.label [] [ H.text title ]
        , H.input
            [ HA.type_ "range"
            , HA.min (String.fromInt min)
            , HA.max (String.fromInt max)
            , HA.step (String.fromInt step)
            , HA.value (String.fromInt currentVal)
            , Html.Events.on "input" decodeEvent
            ]
            []
        , H.text (String.fromInt currentVal)
        ]


satNameFilter : Html Msg
satNameFilter =
    H.div [ HA.class "form-group" ]
        [ H.label [] [ H.text "Satellite" ]
        , H.input
            [ HA.class "form-control"
            , Html.Events.on "input"
                (JD.map SatName Html.Events.targetValue)
            ]
            []
        ]



-- Helper


pred : Time.Zone -> Model -> (Pass -> Bool)
pred timezone filter pass =
    let
        startHour =
            Time.toHour timezone pass.startTime
    in
    List.all identity
        [ startHour >= filter.afterHour
        , startHour <= filter.beforeHour
        , ceiling pass.maxEl >= floor filter.minEl
        , String.contains (String.toUpper filter.satName) (String.toUpper pass.satName)
        ]
