module PassFilter (Model, Action, init, update, view, pred) where

import Date
import Html as H exposing (Html)
import Html.Attributes as HA
import Html.Events
import Json.Decode as JD
import Signal exposing (Address)
import String
import Types exposing (Deg, Pass, SatName)


type alias Hour =
  Int


type alias Model =
  { satName : Maybe String
  , minEl : Deg
  , afterHour : Hour
  , beforeHour : Hour
  }


type Action
  = SatName (Maybe String)
  | MinEl Deg
  | AfterHour Hour
  | BeforeHour Hour
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


view : Address Action -> List SatName -> Model -> Html
view addr satList filter =
  H.div
    []
    [ H.div
        [ HA.class "row"
        , HA.style [ ( "margin-top", "15px" ) ]
        ]
        [ H.div
            [ HA.class "col-xs-6" ]
            [ slider
                addr
                "Start hour"
                AfterHour
                0
                23
                filter.afterHour
            ]
        , H.div
            [ HA.class "col-xs-6" ]
            [ slider
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
            [ slider
                addr
                "Min El"
                MinEl
                30
                89
                filter.minEl
            ]
        , H.div
            [ HA.class "col-xs-4" ]
            [ satSelect addr satList ]
        , H.div
            [ HA.class "col-xs-4" ]
            [ H.label [] []
            , H.button
                [ HA.class "btn btn-primary"
                , HA.type' "submit"
                , HA.style [ ( "display", "block" ), ( "width", "100%" ) ]
                , Html.Events.onClick addr Reset
                ]
                [ H.text "Reset" ]
            ]
        ]
    ]


slider : Signal.Address a -> String -> (Int -> a) -> Int -> Int -> Int -> Html
slider addr title action min max currentVal =
  let
    decodeEvent =
      JD.customDecoder
        (JD.at [ "target", "value" ] JD.string)
        (String.toInt >> Result.map action)
  in
    H.div
      [ HA.class "form-group" ]
      [ H.label [] [ H.text title ]
      , H.input
          [ HA.type' "range"
          , HA.min (toString min)
          , HA.max (toString max)
          , HA.step "1"
          , HA.value (toString currentVal)
          , Html.Events.on "input" decodeEvent (Signal.message addr)
          ]
          []
      , H.text (toString currentVal)
      ]


satSelect : Address Action -> List SatName -> Html
satSelect addr sats =
  let
    toOption sat =
      H.option
        [ HA.value sat ]
        [ H.text sat ]

    optionValToAction val =
      if val == "Any" then
        SatName Nothing
      else
        SatName (Just val)

    decodeEvent =
      JD.customDecoder
        (JD.at [ "target", "value" ] JD.string)
        (optionValToAction >> Ok)
  in
    H.div
      [ HA.class "form-group" ]
      [ H.label [] [ H.text "Sat" ]
      , H.select
          [ HA.class "form-control"
          , Html.Events.on "change" decodeEvent (Signal.message addr)
          ]
          (List.map toOption ("Any" :: sats))
      ]


pred : Model -> (Pass -> Bool)
pred filter pass =
  List.all
    identity
    [ Date.hour (Date.fromTime pass.startTime) >= filter.afterHour
    , Date.hour (Date.fromTime pass.startTime) <= filter.beforeHour
    , pass.maxEl >= filter.minEl
    , filter.satName
        |> Maybe.map ((==) pass.satName)
        |> Maybe.withDefault True
    ]
