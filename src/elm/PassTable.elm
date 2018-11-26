module PassTable exposing (view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Time
import Types exposing (..)


view : Time.Zone -> Time.Posix -> List Pass -> Html a
view timezone time passes =
    let
        sortedPasses =
            List.sortBy (\p -> Time.posixToMillis p.startTime) passes
    in
    case sortedPasses of
        [] ->
            p [ style "text-align" "center" ]
                [ text "No passes" ]

        _ ->
            table
                [ class "table"
                , style "text-align" "center"
                ]
                [ tableHead
                , tbody [] (List.map (passRow timezone time) sortedPasses)
                ]


tableHead : Html a
tableHead =
    let
        th_ txt =
            th [ style "text-align" "center" ]
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


passRow : Time.Zone -> Time.Posix -> Pass -> Html a
passRow timezone time pass =
    let
        timeMillis =
            Time.posixToMillis time

        startMillis =
            Time.posixToMillis pass.startTime

        endMillis =
            Time.posixToMillis pass.endTime

        rowClass =
            if endMillis < timeMillis then
                "text-muted active"

            else if startMillis < timeMillis && timeMillis < endMillis then
                "info"

            else if 70 <= pass.maxEl then
                "danger"

            else if 50 <= pass.maxEl then
                "warning"

            else
                ""

        td_ str =
            td [] [ text str ]

        showDegrees deg =
            deg |> ceiling |> String.fromInt |> (\s -> s ++ "°")

        dayStr =
            Time.toWeekday timezone >> Debug.toString

        startApogeeEnd =
            showTime timezone pass.startTime
                ++ " → "
                ++ showTime timezone pass.apogeeTime
                ++ " → "
                ++ showTime timezone pass.endTime
    in
    tr [ class rowClass ]
        [ td_ (dayStr pass.startTime)
        , td_ (dateStr timezone pass.startTime)
        , td [] [ strong [] [ text pass.satName ] ]
        , td_ (showDegrees pass.maxEl)
        , td_ startApogeeEnd
        , td_ (showDegrees pass.startAz ++ " → " ++ showDegrees pass.endAz)
        ]


dateStr timezone t =
    Debug.toString (Time.toMonth timezone t) ++ " " ++ Debug.toString (Time.toDay timezone t)
