module PassTable (view) where

import Date exposing (Date)
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import String
import Time exposing (Time)
import Types exposing (Deg, LookAngle, Pass, SatName)


view : Time -> List Pass -> Dict SatName LookAngle -> Html
view time passes lookAngles =
  div
    []
    [ table
        [ class "table"
        , style [ ( "text-align", "center" ) ]
        ]
        [ tableHead
        , tbody [] (List.map (passRow time lookAngles) passes)
        ]
    ]


tableHead : Html
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
          [ th' "Day"
          , th' "Satellite"
          , th' "El"
          , th' "Start"
          , th' "Apogee"
          , th' "End"
          , th' "Start Az"
          , th' "End Az"
          ]
      ]


passRow : Time -> Dict SatName LookAngle -> Pass -> Html
passRow time lookAngles pass =
  let
    td' str =
      td [] [ (text str) ]

    showDegrees deg =
      deg |> ceiling |> toString |> \s -> s ++ "°"

    showDayOfWeek time =
      time |> Date.fromTime |> Date.dayOfWeek |> toString

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

    currentAngle =
      Dict.get pass.uid lookAngles

    risingSettingArrow =
      if time <= pass.apogeeTime then
        "↑"
      else
        "↓"

    el =
      case currentAngle of
        Just { elevation, azimuth } ->
          showDegrees elevation
            ++ " ("
            ++ showDegrees pass.maxEl
            ++ ") "
            ++ risingSettingArrow

        Nothing ->
          showDegrees pass.maxEl

    rowClass =
      if time > pass.endTime then
        "text-muted active"
      else if currentAngle /= Nothing then
        "info"
      else if pass.maxEl >= 70 then
        "danger"
      else if pass.maxEl >= 50 then
        "warning"
      else
        ""
  in
    tr
      [ class rowClass ]
      [ td' (showDayOfWeek pass.startTime)
      , td [] [ strong [] [ text pass.satName ] ]
      , td' el
      , td' (showTime pass.startTime)
      , td' (showTime pass.apogeeTime)
      , td' (showTime pass.endTime)
      , td' (showDegrees pass.startAz)
      , td' (showDegrees pass.endAz)
      ]
