port module LookAngleTable exposing (view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Time exposing (Time)
import Types exposing (..)


view : Time -> List ( Pass, LookAngle ) -> Html a
view time lookAngles =
    case lookAngles of
        [] ->
            p [ style [ ( "text-align", "center" ) ] ]
                [ text "None" ]

        _ ->
            div []
                [ table
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
        th_ txt =
            th [ style [ ( "text-align", "center" ) ] ]
                [ text txt ]
    in
        thead []
            [ tr []
                [ th_ "Satellite"
                , th_ "El"
                , th_ "Start"
                , th_ "Apogee"
                , th_ "End"
                , th_ "Start Az"
                , th_ "Az"
                , th_ "End Az"
                ]
            ]


passRow : Time -> ( Pass, LookAngle ) -> Html a
passRow time ( pass, lookAngle ) =
    let
        showDegrees deg =
            deg |> toString |> \s -> s ++ "°"

        risingSettingArrow =
            if time <= pass.apogeeTime then
                "↑"
            else
                "↓"

        elText =
            showDegrees (ceiling lookAngle.elevation)
                ++ " ("
                ++ showDegrees pass.maxEl
                ++ ") "
                ++ risingSettingArrow

        rowClass =
            "success"

        td_ str =
            td [] [ (text str) ]
    in
        tr [ class rowClass ]
            [ td [] [ strong [] [ text pass.satName ] ]
            , td_ elText
            , td_ (showTime pass.startTime)
            , td_ (showTime pass.apogeeTime)
            , td_ (showTime pass.endTime)
            , td_ (showDegrees pass.startAz)
            , td_ (showDegrees (ceiling lookAngle.azimuth))
            , td_ (showDegrees pass.endAz)
            ]
