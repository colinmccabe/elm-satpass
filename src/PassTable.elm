module PassTable (view) where

import Date exposing (Date)
import Html exposing (..)
import Html.Attributes exposing (..)
import Model exposing (Model, Pass, Tle)
import String


view : Model -> Html
view model =
    let baseFilterFn pass =
            let passHour = Date.hour pass.startTime
            in
                (passHour > model.startHour)
                    && (passHour < model.endHour)
                    && (pass.maxEl > model.minEl)
        filterFn =
            case model.satFilter of
                Nothing ->
                    baseFilterFn
                Just sat ->
                    (\pass -> baseFilterFn pass && pass.satName == sat) 
        filteredPasses =
            model.passes |> List.filter filterFn
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
            , th [] [text "Start Time"]
            , th [] [text "End Time"]
            , th [] [text "Start Az"]
            , th [] [text "End Az"]
            ]
        ]


passRow : Pass -> Html
passRow pass =
    let tdTxt txt =
            td [] [(text txt)]
    in
        tr
            []
            [ td [] [ strong [] [text pass.satName] ]
            , tdTxt (showDay pass.startTime)
            , tdTxt (showDegrees pass.maxEl)
            , tdTxt (showTime pass.startTime)
            , tdTxt (showTime pass.endTime)
            , tdTxt (showDegrees pass.startAz)
            , tdTxt (showDegrees pass.endAz)
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
        m = date |> Date.minute |> toString |> String.padLeft 2 '0'
        s = date |> Date.second |> toString |> String.padLeft 2 '0'
    in
        h ++ ":" ++ m ++ ":" ++ s
