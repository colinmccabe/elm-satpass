module PassTable (view) where

import Date exposing (Date)
import Html exposing (..)
import Html.Attributes exposing (..)
import Model exposing (Model)
import PassPredictor exposing (Pass)
import String


view : Model -> Html
view model =
    case model.passes of
        Ok passes ->
            passTable model passes
        Err msg ->
            div [] [ text msg ]


passTable : Model -> List Pass -> Html
passTable model passes =
    let baseFilterFn pass =
            let passHour = Date.hour pass.startTime
            in
                (passHour >= model.startHour)
                    && (passHour <= model.endHour)
                    && (pass.maxEl >= model.minEl)
        filterFn =
            case model.satFilter of
                Nothing ->
                    baseFilterFn
                Just sat ->
                    (\pass -> baseFilterFn pass && pass.satName == sat) 
        filteredPasses =
            passes |> List.filter filterFn
    in
        div
            []
            [ table
                [ class "table table-striped" ]
                [ tableHead
                , tbody [] (List.map passRow filteredPasses)
                ]
            ]


tableHead : Html
tableHead =
    thead
        []
        [ tr
            []
            [ th [] [text "Satellite"]
            , th [] [text "Day"]
            , th [] [text "Max El"]
            , th [] [text "Start"]
            , th [] [text "Apogee"]
            , th [] [text "End"]
            , th [] [text "Start Az"]
            , th [] [text "End Az"]
            ]
        ]


passRow : Pass -> Html
passRow pass =
    let td_ txt =
            td [] [(text txt)]
    in
        tr
            []
            [ td [] [ strong [] [text pass.satName] ]
            , td_ (showDay pass.startTime)
            , td_ (showDegrees pass.maxEl)
            , td_ (showTime pass.startTime)
            , td_ (showTime pass.apogeeTime)
            , td_ (showTime pass.endTime)
            , td_ (showDegrees pass.startAz)
            , td_ (showDegrees pass.endAz)
            ]


showDegrees : Int -> String
showDegrees deg =
    toString deg ++ "°"


showDay : Date -> String
showDay date =
    toString (Date.dayOfWeek date)


showTime : Date -> String
showTime date =
    let h = date |> Date.hour   |> toString
        mm = date |> Date.minute |> toString |> String.padLeft 2 '0'
    in
        h ++ ":" ++ mm
