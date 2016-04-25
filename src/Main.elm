module Main (main) where

import Effects
import Html
import SatPass
import StartApp
import Task
import Time
import Satellite exposing (Coords)


port init : Signal Coords


app : StartApp.App SatPass.Model
app =
  StartApp.start
    { init = SatPass.init
    , update = SatPass.update
    , view = SatPass.view
    , inputs =
        [ init
            |> Time.timestamp
            |> Signal.map SatPass.Init
        , Time.every (1 * Time.second)
            |> Signal.map SatPass.Tick
        ]
    }


main : Signal Html.Html
main =
  app.html


port tasks : Signal (Task.Task Effects.Never ())
port tasks =
  app.tasks
