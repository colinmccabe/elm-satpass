module PassFilter exposing (Model, Msg, init, update, view, pred)

import Date
import Html as H exposing (Html)
import Html.Attributes as HA
import Html.Events
import Json.Decode as JD
import String
import Satellite exposing (Deg, Pass, SatName)


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
    , afterHour = 6
    , beforeHour = 20
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


view : List SatName -> Model -> Html Msg
view satList filter =
    H.div []
        [ H.div
            [ HA.class "row"
            , HA.style [ ( "margin-top", "15px" ) ]
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
                    , HA.type' "submit"
                    , HA.style [ ( "display", "block" ), ( "width", "100%" ) ]
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
            JD.customDecoder (JD.at [ "target", "value" ] JD.string)
                (String.toInt >> Result.map action)
    in
        H.div [ HA.class "form-group" ]
            [ H.label [] [ H.text title ]
            , H.input
                [ HA.type' "range"
                , HA.min (toString min)
                , HA.max (toString max)
                , HA.step (toString step)
                , HA.value (toString currentVal)
                , Html.Events.on "input" decodeEvent
                ]
                []
            , H.text (toString currentVal)
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


pred : Model -> (Pass -> Bool)
pred filter pass =
    List.all identity
        [ Date.hour (Date.fromTime pass.startTime) >= filter.afterHour
        , Date.hour (Date.fromTime pass.startTime) <= filter.beforeHour
        , (ceiling pass.maxEl) >= (floor filter.minEl)
        , String.contains (String.toUpper filter.satName)
            (String.toUpper pass.satName)
        ]
