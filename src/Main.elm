module Main (..) where

import Effects
import Html
import SatPass
import StartApp
import Task
import Time
import Types exposing (..)


-- Ports


port init : Signal Coords


passesReqMailbox : Signal.Mailbox PassReq
passesReqMailbox =
  Signal.mailbox
    { coords = { latitude = 0.0, longitude = 0.0 }
    , begin = 0.0
    , duration = 0.0
    , tles = []
    }


port passesReq : Signal PassReq
port passesReq =
  passesReqMailbox.signal


port passesResp : Signal (List Pass)


lookAngleReqMailbox : Signal.Mailbox LookAngleReq
lookAngleReqMailbox =
  Signal.mailbox
    { coords = { latitude = 0.0, longitude = 0.0 }
    , time = 0.0
    , tles = []
    }


port lookAngleReq : Signal LookAngleReq
port lookAngleReq =
  lookAngleReqMailbox.signal


port lookAngleResp : Signal (List ( String, LookAngle ))



-- StartApp


app : StartApp.App SatPass.Model
app =
  StartApp.start
    { init = SatPass.init
    , update =
        SatPass.update
          { passesReq = passesReqMailbox
          , lookAngleReq = lookAngleReqMailbox
          }
    , view = SatPass.view
    , inputs =
        [ init
            |> Time.timestamp
            |> Signal.map SatPass.Init
        , passesResp
            |> Signal.map SatPass.Passes
        , Time.every (1 * Time.second)
            |> Signal.map SatPass.Tick
        , lookAngleResp
            |> Signal.map SatPass.LookAngles
        ]
    }


main : Signal Html.Html
main =
  app.html


port tasks : Signal (Task.Task Effects.Never ())
port tasks =
  app.tasks
