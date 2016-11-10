module PassTable exposing (view)

import Date exposing (Date)
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
        th_ txt =
            th [ style [ ( "text-align", "center" ) ] ]
                [ text txt ]
    in
        thead []
            [ tr []
                [ th_ "Day"
                , th_ "Date"
                , th_ "Satellite"
                , th_ "Max El"
                , th_ "Start → Apogee → End"
                , th_ "Az"
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

        td_ str =
            td [] [ (text str) ]

        showDegrees deg =
            deg |> ceiling |> toString |> (\s -> s ++ "°")

        dayStr =
            Date.fromTime >> Date.dayOfWeek >> toString

        dateStr =
            Date.fromTime >> \d -> (toString (Date.month d)) ++ " " ++ (toString (Date.day d))

        startApogeeEnd =
            showTime pass.startTime
                ++ " → "
                ++ showTime pass.apogeeTime
                ++ " → "
                ++ showTime pass.endTime
    in
        tr [ class rowClass ]
            [ td_ (dayStr pass.startTime)
            , td_ (dateStr pass.startTime)
            , td [] [ strong [] [ text pass.satName ] ]
            , td_ (showDegrees pass.maxEl)
            , td_ startApogeeEnd
            , td_ (showDegrees pass.startAz ++ " → " ++ showDegrees pass.endAz)
            ]
