module PassTable exposing (view)

import Date exposing (Date)
import Html exposing (..)
import Html.Attributes exposing (..)
import String
import Time exposing (Time)
import Types exposing (..)


view : Time -> List Pass -> Html a
view time passes =
    let
        sortedPasses =
            List.sortBy .startTime passes
    in
        table
            [ class "table"
            , style [ ( "text-align", "center" ) ]
            ]
            [ tableHead
            , tbody [] (List.map (passRow time) sortedPasses)
            ]


tableHead : Html a
tableHead =
    let
        th' txt =
            th [ style [ ( "text-align", "center" ) ] ]
                [ text txt ]
    in
        thead []
            [ tr []
                [ th' "Day"
                , th' "Satellite"
                , th' "Max El"
                , th' "Start"
                , th' "Apogee"
                , th' "End"
                , th' "Start Az"
                , th' "End Az"
                ]
            ]


passRow : Time -> Pass -> Html a
passRow time pass =
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

        rowClass =
            if time > pass.endTime then
                "text-muted active"
            else if time > pass.startTime && time < pass.endTime then
                "info"
            else if pass.maxEl >= 70 then
                "danger"
            else if pass.maxEl >= 50 then
                "warning"
            else
                ""
    in
        tr [ class rowClass ]
            [ td' (showDayOfWeek pass.startTime)
            , td [] [ strong [] [ text pass.satName ] ]
            , td' (showDegrees pass.maxEl)
            , td' (showTime pass.startTime)
            , td' (showTime pass.apogeeTime)
            , td' (showTime pass.endTime)
            , td' (showDegrees pass.startAz)
            , td' (showDegrees pass.endAz)
            ]
