module SatPass where

import Action as A exposing (Action)
import Effects exposing (Effects)
import Html as H exposing (Html)
import Html.Attributes as HA
import Html.Events
import Http
import Model exposing (Model)
import PassPredictor
import PassTable
import SatSelect
import Signal
import Slider
import StartApp
import Task exposing (Task)
import Time exposing (Time)


duration : Time
duration =
    48 * Time.hour


mySats : List String
mySats =
    [ "FO-29", "XW-2F", "NO-44", "SO-50", "AO-73", "NO-84", "AO-85"
    , "RS-15", "XW-2A", "XW-2B", "XW-2C", "XW-2D", "ISS"
    ]


-- StartApp

app : StartApp.App Model
app =
    StartApp.start
        { init = init
        , inputs =
            [ initSignal
                |> Time.timestamp
                |> Signal.map (fst >> A.Init)
            ]
        , update = update
        , view = view
        }


main : Signal Html
main =
    app.html


port tasks : Signal (Task.Task Effects.Never ())
port tasks =
    app.tasks


port initSignal : Signal Bool


-- Model

init : (Model, Effects Action)
init =
    ( { passes = Err "Loading..."
      , satFilter = Nothing
      , minEl = 30
      , startHour = 6
      , endHour = 22
      }
    , Effects.none
    )


-- Update

update : Action -> Model -> (Model, Effects Action)
update action model =
    case action of
        A.Init timestamp ->
            ( model
            , Effects.task (getPasses mySats timestamp duration)
            )

        A.Passes passes ->
            ( { model | passes = passes}
            , Effects.none
            )

        A.FilterSat satMay ->
            ( { model | satFilter = satMay }, Effects.none )

        A.FilterMinEl el ->
            ( { model | minEl = el }, Effects.none )

        A.FilterStartHour hr ->
            ( { model | startHour = hr }, Effects.none )

        A.FilterEndHour hr ->
            ( { model | endHour = hr }, Effects.none )

        A.FilterReset ->
            ( { model
                | satFilter = Nothing
                , minEl = 30
                , startHour = 6
                , endHour = 22
              }
            , Effects.none
            )


-- View

view : Signal.Address Action -> Model -> Html
view addr model =
    H.div
        [ HA.class "container" ]
        [ H.div
            [ HA.class "row"
            , HA.style [("margin-top", "15px")]]
            [ H.div
                [ HA.class "col-xs-6" ]
                [ Slider.view
                    addr
                    "Start hour"
                    A.FilterStartHour
                    0
                    23
                    model.startHour
                ]
            , H.div
                [ HA.class "col-xs-6" ]
                [ Slider.view
                    addr
                    "End hour"
                    A.FilterEndHour
                    0
                    23
                    model.endHour
                ]
            ]
        , H.div
            [ HA.class "row" ]
            [ H.div
                [ HA.class "col-xs-4" ]
                [ Slider.view addr "Min El" A.FilterMinEl 30 89 model.minEl ]
            , H.div
                [ HA.class "col-xs-4" ]
                [ SatSelect.view addr mySats ]
            , H.div
                [ HA.class "col-xs-4" ]
                [ H.label [] []
                , H.button
                    [ HA.class "btn btn-primary"
                    , HA.type' "submit"
                    , HA.style [("display", "block"), ("width", "100%")]
                    , Html.Events.onClick addr A.FilterReset
                    ]
                    [ H.text "Reset" ]
                ]
            ]
        , PassTable.view model
        ]


-- tasks

getPasses : List String -> Time -> Time -> Task a Action
getPasses sats from duration =
    Http.getString "nasabare.txt"
        |> Task.mapError toString
        |> (flip Task.andThen) (PassPredictor.getPasses sats from duration)
        |> Task.mapError (\msg -> "Could not calculate passes: " ++ msg)
        |> Task.toResult
        |> Task.map A.Passes
