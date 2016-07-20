module PassTable exposing (view)

import Date exposing (Date)
import Date.Format
import Html exposing (..)
import Html.Attributes exposing (..)
import Time exposing (Time)
import Types exposing (..)


view : Time -> List Pass -> Html a
view time passes =
    let
        sortedPasses =
            List.sortBy .startTime passes
    in
        case sortedPasses of
            [] ->
                p [ style [ ( "text-align", "center" ) ] ]
                    [ text "No passes" ]

            _ ->
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
                , th' "Date"
                , th' "Satellite"
                , th' "Max El"
                , th' "Start → Apogee → End"
                , th' "Az"
                ]
            ]


passRow : Time -> Pass -> Html a
passRow time pass =
    let
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

        td' str =
            td [] [ (text str) ]

        showDegrees deg =
            deg |> ceiling |> toString |> (\s -> s ++ "°")

        dayStr =
            Date.fromTime >> Date.Format.format "%a"

        dateStr =
            Date.fromTime >> Date.Format.format "%m/%d"

        timeStr =
            Date.fromTime >> Date.Format.format "%k:%M"

        startApogeeEnd =
            timeStr pass.startTime
                ++ " → "
                ++ timeStr pass.apogeeTime
                ++ " → "
                ++ timeStr pass.endTime
    in
        tr [ class rowClass ]
            [ td' (dayStr pass.startTime)
            , td' (dateStr pass.startTime)
            , td [] [ strong [] [ text pass.satName ] ]
            , td' (showDegrees pass.maxEl)
            , td' startApogeeEnd
            , td' (showDegrees pass.startAz ++ " → " ++ showDegrees pass.endAz)
            ]
