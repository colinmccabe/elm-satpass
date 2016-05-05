module LookAngleTable exposing (Model, init, Msg, update, view, subs)

import Date
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import String
import Task exposing (Task)
import Time exposing (Time)
import Satellite exposing (..)


type alias Model =
  List ( Pass, LookAngle )


init : Model
init =
  []


type Msg
  = GetLookAngles Time
  | LookAngles (List ( Pass, LookAngle ))
  | Fail String


type alias Context a =
  { a
    | coords : Coords
    , tles : Dict String Tle
    , passes : List Pass
  }


update : Context a -> Msg -> Model -> ( Model, Cmd Msg )
update context action model =
  case action of
    GetLookAngles time ->
      ( model
      , getLookAngles context time
      )

    LookAngles newLookAngles ->
      ( newLookAngles
      , Cmd.none
      )

    Fail _ ->
      ( model, Cmd.none )


view : Time -> Model -> Html a
view time lookAngles =
  case lookAngles of
    [] ->
      div [] []

    _ ->
      div
        []
        [ h3
            [ style [ ( "text-align", "center" ) ] ]
            [ Html.text "Passing now" ]
        , table
            [ class "table"
            , style [ ( "text-align", "center" ) ]
            ]
            [ tableHead
            , tbody [] (List.map (passRow time) lookAngles)
            ]
        ]


tableHead : Html a
tableHead =
  let
    th' txt =
      th
        [ style [ ( "text-align", "center" ) ] ]
        [ text txt ]
  in
    thead
      []
      [ tr
          []
          [ th' "Satellite"
          , th' "El"
          , th' "Start"
          , th' "Apogee"
          , th' "End"
          , th' "Start Az"
          , th' "Az"
          , th' "End Az"
          ]
      ]


passRow : Time -> ( Pass, LookAngle ) -> Html a
passRow time ( pass, lookAngle ) =
  let
    td' str =
      td [] [ (text str) ]

    showDegrees deg =
      deg |> ceiling |> toString |> \s -> s ++ "°"

    showTime time =
      let
        h =
          time |> Date.fromTime |> Date.hour |> toString

        mm =
          time
            |> Date.fromTime
            |> Date.minute
            |> toString
            |> String.padLeft 2 '0'
      in
        h ++ ":" ++ mm

    risingSettingArrow =
      if time <= pass.apogeeTime then
        "↑"
      else
        "↓"

    elText =
      showDegrees lookAngle.elevation
        ++ " ("
        ++ showDegrees pass.maxEl
        ++ ") "
        ++ risingSettingArrow

    rowClass =
      "success"
  in
    tr
      [ class rowClass ]
      [ td [] [ strong [] [ text pass.satName ] ]
      , td' elText
      , td' (showTime pass.startTime)
      , td' (showTime pass.apogeeTime)
      , td' (showTime pass.endTime)
      , td' (showDegrees pass.startAz)
      , td' (showDegrees lookAngle.azimuth)
      , td' (showDegrees pass.endAz)
      ]


-- Subscriptions


subs : Model -> Sub Msg
subs model =
  Time.every Time.second GetLookAngles


-- Cmds


getLookAngles : Context r -> Time -> Cmd Msg
getLookAngles { coords, tles, passes } time =
  let
    getLookAngle pass tle =
      Satellite.getLookAngle coords tle time
        |> Task.map (\lookAngle -> ( pass, lookAngle ))
  in
    passes
      |> List.filter (\pass -> time > pass.startTime && time < pass.endTime)
      |> List.filterMap
          (\pass -> Dict.get pass.satName tles |> Maybe.map (getLookAngle pass))
      |> Task.sequence
      |> Task.perform Fail LookAngles
