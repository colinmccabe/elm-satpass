module LookAngleTable exposing (view)

import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Time
import Types exposing (..)


view : Timestamp -> Dict PassId Pass -> Dict PassId LookAngle -> Html a
view time passes lookAngles =
    let
        passAnglePairs =
            Dict.merge (\_ _ l -> l)
                (\_ lookAngle pass l -> l ++ [ ( lookAngle, pass ) ])
                (\_ _ l -> l)
                lookAngles
                passes
                []
    in
        case passAnglePairs of
            [] ->
                p [ style "text-align" "center" ]
                    [ text "None" ]

            _ ->
                div []
                    [ table
                        [ class "table"
                        , style "text-align" "center"
                        ]
                        [ tableHead
                        , tbody [] (List.map (passRow time) passAnglePairs)
                        ]
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


passRow : Timestamp -> ( LookAngle, Pass ) -> Html a
passRow time ( lookAngle, pass ) =
    let
        showDegrees deg =
            deg |> ceiling |> String.fromInt |> \s -> s ++ "°"

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
            , td_ (showDegrees lookAngle.azimuth)
            , td_ (showDegrees pass.endAz)
            ]
