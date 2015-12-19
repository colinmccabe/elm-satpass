module SatPass where

{- @docs main -}

import Action as A exposing (Action)
import Date
import Html exposing (Html)
import Html.Attributes as HtmlAttr
import Html.Events
import Http
import Model exposing (Model, Pass, PassRequest, Tle)
import PassTable
import SatSelect
import Set
import Signal
import Signal.Time
import Slider
import String
import Task exposing (Task)
import Time exposing (Time)


mySats : List String
mySats =
    [ "FO-29", "XW-2F", "NO-44", "SO-50", "AO-73", "NO-84", "AO-85"
    , "RS-15", "ISS"
    ]


-- Wiring

events : Signal.Mailbox Action
events =
    Signal.mailbox A.NoOp


model : Signal Model
model =
    let actions =
            Signal.merge events.signal predictions
    in
        actions |> Signal.foldp update init


{-|-}
main : Signal Html
main =
    model |> Signal.map (view events.address)


-- prediction pipeline

passesReq : Time -> Result a (List (String, Tle)) -> PassRequest
passesReq startTime tleResult =
    { start = startTime
    , duration = 3 * 24 * Time.hour
    , tle =
        case tleResult of
            Ok tle -> tle
            Err _ -> []
    }


getTleTask : Time -> Task a PassRequest
getTleTask startTime =
    Http.getString "nasabare.txt"
        |> Task.map parseTle
        |> Task.map (List.filter (\(sat, _) -> List.member sat mySats))
        |> Task.toResult
        |> Task.map (passesReq startTime)


port tleRequests : Signal (Task a ())
port tleRequests =
    Signal.Time.startTime
        |> Signal.map getTleTask
        |> Signal.map ((flip Task.andThen) (Signal.send tleResponses.address))


tleResponses : Signal.Mailbox PassRequest
tleResponses =
    Signal.mailbox { start = 0.0, duration = 0.0, tle = [] }


type alias JsPredictionRequest =
    { start : Float
    , duration : Float
    , tle : List (String, Tle)
    }


port jsPredictionRequests : Signal JsPredictionRequest
port jsPredictionRequests =
    let toJson req =
            { req
                | start = req.start * Time.millisecond
                , duration = req.duration * Time.millisecond
            }
    in
        tleResponses.signal |> Signal.map toJson 


type alias JsPass =
    { satName : String
    , maxEl : Int
    , startTime : Float
    , endTime : Float
    , startAz : Int
    , endAz : Int
    }


port jsPredictions : Signal (List JsPass)


predictions : Signal Action
predictions =
    let toPass passJs =
            { passJs
                | startTime = Date.fromTime (passJs.startTime * Time.millisecond)
                , endTime = Date.fromTime (passJs.endTime * Time.millisecond)
            }
    in
        jsPredictions
            |> Signal.map (List.map toPass)
            |> Signal.map A.Passes


-- Model

init : Model
init =
    { passes = []
    , sats = Set.empty
    , satFilter = Nothing
    , minEl = 30
    , startHour = 6
    , endHour = 22
    }


-- Update

update : Action -> Model -> Model
update action model =
    case action of
        A.Passes ps ->
            { model
                | passes = ps
                , sats = ps |> List.map .satName |> Set.fromList
            }

        A.FilterSat satMay ->
            { model | satFilter = satMay }

        A.FilterMinEl el ->
            { model | minEl = el }

        A.FilterStartHour hr ->
            { model | startHour = hr }

        A.FilterEndHour hr ->
            { model | endHour = hr }

        A.FilterReset ->
            { model
                | satFilter = Nothing
                , minEl = 30
                , startHour = 6
                , endHour = 22
            }

        A.NoOp ->
            model


-- View

view : Signal.Address Action -> Model -> Html.Html
view addr model =
    Html.div
        [ HtmlAttr.class "container" ]
        [ Html.div
            [ HtmlAttr.class "row"
            , HtmlAttr.style [("margin-top", "15px")]]
            [ Html.div
                [ HtmlAttr.class "col-xs-6" ]
                [ Slider.view
                    addr
                    "Start hour"
                    A.FilterStartHour
                    0 23 6
                    model.startHour
                ]
            , Html.div
                [ HtmlAttr.class "col-xs-6" ]
                [ Slider.view
                    addr
                    "End hour"
                    A.FilterEndHour
                    0 23 22
                    model.endHour
                ]
            ]
        , Html.div
            [ HtmlAttr.class "row" ]
            [ Html.div
                [ HtmlAttr.class "col-xs-4" ]
                [ Slider.view addr "Min El" A.FilterMinEl 30 89 30 model.minEl ]
            , Html.div
                [ HtmlAttr.class "col-xs-4" ]
                [ SatSelect.view addr model ]
            , Html.div
                [ HtmlAttr.class "col-xs-4" ]
                [ Html.label [] []
                , Html.button
                    [ HtmlAttr.class "btn btn-primary"
                    , HtmlAttr.type' "submit"
                    , HtmlAttr.style [("display", "block"), ("width", "100%")]
                    , Html.Events.onClick addr A.FilterReset
                    ]
                    [ Html.text "Reset" ]
                ]
            ]
        , PassTable.view model
        ]


-- etc

parseTle : String -> List (String, Tle)
parseTle rawTle =
    rawTle
        |> String.split "\n"
        |> toAssocList


toAssocList : List String -> List (String, Tle)
toAssocList list =
    case list of
        satName :: tle1 :: tle2 :: rest ->
            (satName, {line1 = tle1, line2 = tle2}) :: toAssocList rest
        _ ->
            []
