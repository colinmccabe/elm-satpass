module SatPass where

import Date exposing (Date)
import Effects exposing (Effects)
import PassFilter
import Html exposing (Html, div)
import Html.Attributes exposing (class)
import Http
import PassTable
import Signal
import StartApp
import String
import Task exposing (Task)
import Time exposing (Time)
import Types exposing (..)


duration : Time
duration =
    48 * Time.hour


sats : List String
sats =
    [ "FO-29", "XW-2F", "NO-44", "SO-50", "AO-73", "NO-84", "AO-85"
    , "RS-15", "XW-2A", "XW-2B", "XW-2C", "XW-2D", "ISS"
    ]


-- Model

type alias Model =
    { passes : List Pass
    , filter : PassFilter.Model
    }


init : (Model, Effects Action)
init =
    ( { passes = []
      , filter = PassFilter.init
      }
    , Effects.none
    )


-- Action

type Action
    = Init Time
    | Passes (List Pass)
    | Filter PassFilter.Action
    | NoOp


-- Update

update : Action -> Model -> (Model, Effects Action)
update action model =
    case action of
        Init timestamp ->
            ( model
            , Effects.task (requestPasses sats timestamp duration)
            )

        Passes passes ->
            ( { model | passes = passes }
            , Effects.none
            )

        Filter action ->
            ( { model | filter = PassFilter.update action model.filter }
            , Effects.none
            )

        NoOp ->
            ( model, Effects.none )


-- View

view : Signal.Address Action -> Model -> Html
view addr model =
    div
        [ class "container" ]
        [ PassFilter.view
            (Signal.forwardTo addr Filter)
            sats
            model.filter
        , PassTable.view model.filter model.passes
        ]


-- Wiring

app : StartApp.App Model
app =
    StartApp.start
        { init = init
        , update = update
        , view = view
        , inputs =
            [ initSignalIn
                |> Time.timestamp
                |> Signal.map (fst >> Init)
            , passesIn
                |> Signal.map (List.map elmPass)
                |> Signal.map Passes
            ]
        }


main : Signal Html
main =
    app.html


port tasks : Signal (Task.Task Effects.Never ())
port tasks =
    app.tasks


port initSignalIn : Signal Bool


passReqMailbox : Signal.Mailbox PassReq
passReqMailbox =
    Signal.mailbox
        { tles = []
        , begin = 0
        , duration = 0
        }


port passReqOut : Signal PassReq
port passReqOut =
    passReqMailbox.signal


port passesIn : Signal (List JsPass)


-- Interop

type alias PassReq =
    { tles : List Tle
    , begin : Float
    , duration : Float
    }


type alias JsPass =
    { satName : String
    , maxEl : Int
    , startTime : Float
    , apogeeTime : Float
    , endTime : Float
    , startAz : Int
    , endAz : Int
    }


requestPasses : List String -> Time -> Time -> Task a Action
requestPasses sats begin duration =
    Http.getString "nasabare.txt"
        |> Task.map
            (\rawTle ->
                { tles = tles rawTle sats
                , begin = Time.inMilliseconds begin
                , duration = Time.inMilliseconds duration
                })
        |> (flip Task.andThen) (Signal.send passReqMailbox.address)
        |> Task.toResult
        |> Task.map (\_ -> NoOp)


tles : String -> List SatName -> List Tle
tles rawTle desiredSats =
    rawTle
        |> String.split "\n"
        |> parseTleLines
        |> List.filter (\tle -> List.member tle.satName desiredSats)


parseTleLines : List String -> List Tle
parseTleLines lines =
    case lines of
        satName :: tle1 :: tle2 :: rest ->
            {satName = satName, line1 = tle1, line2 = tle2} :: parseTleLines rest
        _ ->
            []


elmPass : JsPass -> Pass
elmPass nativePass =
    let toDate jsTime =
            Date.fromTime (jsTime * Time.millisecond)
    in
        { nativePass
            | startTime = toDate nativePass.startTime
            , endTime = toDate nativePass.endTime
            , apogeeTime = toDate nativePass.apogeeTime
        }
