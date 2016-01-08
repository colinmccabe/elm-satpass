module SatPass where

import Effects exposing (Effects)
import PassFilter
import Html as H exposing (Html)
import Html.Attributes as HA
import Http
import PassPredictor
import PassTable
import Signal
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

port initSignal : Signal Bool


app : StartApp.App Model
app =
    StartApp.start
        { init = init
        , inputs =
            [ initSignal
                |> Time.timestamp
                |> Signal.map (fst >> Init)
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


-- Model

type alias Model =
    { passes : Result String (List PassPredictor.Pass)
    , filter : PassFilter.Model
    }


init : (Model, Effects Action)
init =
    ( { passes = Err "Loading..."
      , filter = PassFilter.init
      }
    , Effects.none
    )


-- Action

type Action
    = Init Time
    | Passes (Result String (List PassPredictor.Pass))
    | Filter PassFilter.Action


-- Update

update : Action -> Model -> (Model, Effects Action)
update action model =
    case action of
        Init timestamp ->
            ( model
            , Effects.task (getPasses mySats timestamp duration)
            )

        Passes passes ->
            ( { model | passes = passes }
            , Effects.none
            )

        Filter filterAction ->
            ( { model | filter = PassFilter.update filterAction model.filter }
            , Effects.none
            )


-- View

view : Signal.Address Action -> Model -> Html
view addr model =
    H.div
        [ HA.class "container" ]
        [ PassFilter.view
            (Signal.forwardTo addr Filter)
            mySats
            model.filter
        , PassTable.view model.filter model.passes
        ]


-- Tasks

getPasses : List String -> Time -> Time -> Task a Action
getPasses sats from duration =
    Http.getString "nasabare.txt"
        |> Task.mapError toString
        |> (flip Task.andThen) (PassPredictor.getPasses sats from duration)
        |> Task.mapError (\msg -> "Could not calculate passes: " ++ msg)
        |> Task.toResult
        |> Task.map Passes
