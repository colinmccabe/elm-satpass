module PassFilter where

import Date
import Html as H exposing (Html)
import Html.Attributes as HA
import Html.Events
import PassPredictor exposing (Pass)
import SatSelect
import Signal exposing (Address)
import Slider


type alias Degrees = Int


type alias HourOfDay = Int


type alias Model =
    { satName : Maybe String
    , minEl : Degrees
    , afterHour : HourOfDay
    , beforeHour : HourOfDay
    }


type Action
    = SatName (Maybe String)
    | MinEl Degrees
    | AfterHour HourOfDay
    | BeforeHour HourOfDay
    | Reset


update : Action -> Model -> Model
update filterAction filter =
    case filterAction of
        SatName satMay ->
            { filter | satName = satMay }

        MinEl el ->
            { filter | minEl = el }

        AfterHour hr ->
            { filter | afterHour = hr }

        BeforeHour hr ->
            { filter | beforeHour = hr }

        Reset ->
            init


init : Model
init =
    { satName = Nothing
    , minEl = 30
    , afterHour = 6
    , beforeHour = 18
    }


view : Address Action -> List String -> Model -> Html
view addr satList filter =
    H.div
        []
        [ H.div
            [ HA.class "row"
            , HA.style [("margin-top", "15px")]
            ]
            [ H.div
                [ HA.class "col-xs-6" ]
                [ Slider.view
                    addr
                    "Start hour"
                    AfterHour
                    0
                    23
                    filter.afterHour
                ]
            , H.div
                [ HA.class "col-xs-6" ]
                [ Slider.view
                    addr
                    "End hour"
                    BeforeHour
                    0
                    23
                    filter.beforeHour
                ]
            ]
        , H.div
            [ HA.class "row" ]
            [ H.div
                [ HA.class "col-xs-4" ]
                [ Slider.view
                    addr
                    "Min El"
                    MinEl
                    30
                    89
                    filter.minEl
                ]
            , H.div
                [ HA.class "col-xs-4" ]
                [ SatSelect.view addr SatName satList ]
            , H.div
                [ HA.class "col-xs-4" ]
                [ H.label [] []
                , H.button
                    [ HA.class "btn btn-primary"
                    , HA.type' "submit"
                    , HA.style [("display", "block"), ("width", "100%")]
                    , Html.Events.onClick addr Reset
                    ]
                    [ H.text "Reset" ]
                ]
            ]
        ]


pred : Model -> (Pass -> Bool)
pred filter pass =
    List.all identity
        [ Date.hour pass.startTime >= filter.afterHour
        , Date.hour pass.startTime <= filter.beforeHour
        , pass.maxEl >= filter.minEl
        , filter.satName
            |> Maybe.map ((==) pass.satName)
            |> Maybe.withDefault True
        ]
