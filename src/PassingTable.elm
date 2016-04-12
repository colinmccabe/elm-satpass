module PassingTable (view) where

import Date
import Html exposing (..)
import Html.Attributes exposing (..)
import String
import Time exposing (Time)
import Satellite exposing (..)


view : Time -> List ( LookAngle, Pass ) -> Html
view time lookAngles =
  case lookAngles of
    [] ->
      div [] []

    _ ->
      div
        []
        [ h4 [] [ Html.text "Passing now" ]
        , table
            [ class "table"
            , style [ ( "text-align", "center" ) ]
            ]
            [ tableHead
            , tbody [] (List.map (passRow time) lookAngles)
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


passRow : Time -> ( LookAngle, Pass ) -> Html
passRow time ( lookAngle, pass ) =
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
