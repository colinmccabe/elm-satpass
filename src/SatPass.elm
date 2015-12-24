module SatPass where

import Action as A exposing (Action)
import Effects exposing (Effects)
import Html exposing (Html)
import Html.Attributes as HtmlAttr
import Html.Events
import Http
import Model exposing (Model)
import PassPredictor
import PassTable
import SatSelect
import Set
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
    ( { programStartTime = Nothing
      , passes = Err "Loading..."
      , sats = Set.empty
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
            ( { model | programStartTime = Just timestamp }
            , Effects.task getTle
            )

        A.Tle (Ok rawTle) ->
            case model.programStartTime of
                Just t ->
                    ( model
                    , Effects.task (getPasses rawTle mySats t duration)
                    )
                Nothing ->
                    ( { model | passes = Err "Failed to get current time" }
                    , Effects.none
                    )

        A.Tle (Err msg) ->
            ( { model
                | passes = Err ("Could not get TLE: " ++ (toString msg))
              }
            , Effects.none
            )

        A.Passes (Ok ps) ->
            ( { model
                | passes = Ok ps
                , sats = ps |> List.map .satName |> Set.fromList
              }
            , Effects.none
            )

        A.Passes (Err msg) ->
            ( { model
                | passes = Err ("Could not calculate passes: " ++ (toString msg))
              }
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
                    0
                    23
                    model.startHour
                ]
            , Html.div
                [ HtmlAttr.class "col-xs-6" ]
                [ Slider.view
                    addr
                    "End hour"
                    A.FilterEndHour
                    0
                    23
                    model.endHour
                ]
            ]
        , Html.div
            [ HtmlAttr.class "row" ]
            [ Html.div
                [ HtmlAttr.class "col-xs-4" ]
                [ Slider.view addr "Min El" A.FilterMinEl 30 89 model.minEl ]
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


-- tasks

getTle : Task a Action
getTle =
    Http.getString "nasabare.txt"
        |> Task.toResult
        |> Task.map A.Tle


getPasses : String -> List String -> Time -> Time -> Task a Action
getPasses rawTle desiredSats from duration =
    PassPredictor.getPasses rawTle desiredSats from duration
        |> Task.map A.Passes
