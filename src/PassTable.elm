module PassTable (view) where

import Date exposing (Date)
import PassFilter
import Html exposing (..)
import Html.Attributes exposing (..)
import String
import Types exposing (Deg, Pass)


view : PassFilter.Model -> List Pass -> Html
view filter allPasses =
  case List.filter (PassFilter.pred filter) allPasses of
    [] ->
      div [] [ text "No passes to show yet..." ]

    filteredPasses ->
      passTable filteredPasses


passTable : List Pass -> Html
passTable passes =
  div
    []
    [ table
        [ class "table table-striped" ]
        [ tableHead
        , tbody [] (List.map passRow passes)
        ]
    ]


tableHead : Html
tableHead =
  thead
    []
    [ tr
        []
        [ th [] [ text "Day" ]
        , th [] [ text "Satellite" ]
        , th [] [ text "Max El" ]
        , th [] [ text "Start" ]
        , th [] [ text "Apogee" ]
        , th [] [ text "End" ]
        , th [] [ text "Start Az" ]
        , th [] [ text "End Az" ]
        ]
    ]


passRow : Pass -> Html
passRow pass =
  let
    td' str =
      td [] [ (text str) ]

    showDegrees deg =
      toString deg ++ "°"

    showDay time =
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
  in
    tr
      []
      [ td' (showDay pass.startTime)
      , td [] [ strong [] [ text pass.satName ] ]
      , td' (showDegrees pass.maxEl)
      , td' (showTime pass.startTime)
      , td' (showTime pass.apogeeTime)
      , td' (showTime pass.endTime)
      , td' (showDegrees pass.startAz)
      , td' (showDegrees pass.endAz)
      ]
