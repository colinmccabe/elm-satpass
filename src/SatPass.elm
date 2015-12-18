module SatPass where

{- @docs main -}

import Action exposing (Action)
import Date
import Html exposing (Html)
import Html.Attributes as HtmlAttr
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
    Signal.mailbox Action.NoOp


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

passesReq : Time -> Result Http.Error (List (String, Tle)) -> PassRequest
passesReq startTime tleResult =
    { start = startTime
    , duration = 48 * Time.hour
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
        |> Signal.map ((flip Task.andThen) (\resp -> Signal.send tleResponses.address resp))


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
            |> Signal.map Action.Passes


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
        Action.Passes ps ->
            { model
                | passes = ps
                , sats = ps |> List.map (\pass -> pass.satName) |> Set.fromList
            }

        Action.SatFilter satMay ->
            { model | satFilter = satMay }

        Action.MinEl el ->
            { model | minEl = el }

        Action.StartHour hr ->
            { model | startHour = hr }

        Action.EndHour hr ->
            { model | endHour = hr }

        Action.NoOp ->
            model


-- View

view : Signal.Address Action -> Model -> Html.Html
view addr model =
    Html.div
        [ HtmlAttr.class "container" ]
        [ Html.div
            [ HtmlAttr.class "row" ]
            [ Html.div
                [ HtmlAttr.class "col-xs-6" ]
                [ Slider.view addr model "Start hour" Action.StartHour 0 23 6 ]
            , Html.div
                [ HtmlAttr.class "col-xs-6" ]
                [ Slider.view addr model "End hour" Action.EndHour 0 23 22 ]
            ]
        , Html.div
            [ HtmlAttr.class "row" ]
            [ Html.div
                [ HtmlAttr.class "col-xs-6" ]
                [ Slider.view addr model "Min El" Action.MinEl 30 89 30 ]
            , Html.div
                [ HtmlAttr.class "col-xs-6" ]
                [ SatSelect.view addr model ]
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
