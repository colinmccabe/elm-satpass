module Main (..) where

import Effects exposing (Effects)
import PassFilter
import Html exposing (Html, div)
import Html.Attributes exposing (class)
import Http
import PassTable
import Signal
import StartApp
import Task exposing (Task)
import Time exposing (Time)
import Types exposing (..)


duration : Time
duration =
  48 * Time.hour


sats : List String
sats =
  [ "FO-29"
  , "XW-2F"
  , "NO-44"
  , "SO-50"
  , "AO-73"
  , "NO-84"
  , "AO-85"
  , "RS-15"
  , "XW-2A"
  , "XW-2B"
  , "XW-2C"
  , "XW-2D"
  , "ISS"
  ]


type alias Model =
  { coords : Coords
  , passes : List Pass
  , filter : PassFilter.Model
  }


init : ( Model, Effects Action )
init =
  ( { coords = { latitude = 0.0, longitude = 0.0 }
    , passes = []
    , filter = PassFilter.init
    }
  , Effects.none
  )


type Action
  = Init ( Time, Coords )
  | Passes (List Pass)
  | Filter PassFilter.Action
  | NoOp


update : Action -> Model -> ( Model, Effects Action )
update action model =
  case action of
    Init ( time, coords ) ->
      ( { model | coords = coords }
      , Effects.task (getPasses coords time duration sats)
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



-- Tasks


getPasses : Coords -> Time -> Time -> List SatName -> Task a Action
getPasses coords begin duration sats =
  Http.getString "https://s3.amazonaws.com/cmccabe/keps/nasabare.txt"
    |> Task.map parseTle
    |> Task.map (List.filter (\t -> List.member t.satName sats))
    |> Task.map
        (\tles ->
          { coords = coords
          , begin = Time.inMilliseconds begin
          , duration = Time.inMilliseconds duration
          , tles = tles
          }
        )
    |> (flip Task.andThen) (Signal.send passesOutMailbox.address)
    |> Task.toResult
    |> Task.map (\_ -> NoOp)



-- Ports


port initIn : Signal Coords


passesOutMailbox : Signal.Mailbox PassReq
passesOutMailbox =
  Signal.mailbox
    { coords = { latitude = 0.0, longitude = 0.0 }
    , begin = 0
    , duration = 0
    , tles = []
    }


port passesOut : Signal PassReq
port passesOut =
  passesOutMailbox.signal


port passesIn : Signal (List Pass)



-- StartApp


app : StartApp.App Model
app =
  StartApp.start
    { init = init
    , update = update
    , view = view
    , inputs =
        [ initIn
            |> Time.timestamp
            |> Signal.map Init
        , passesIn
            |> Signal.map Passes
        ]
    }


main : Signal Html
main =
  app.html


port tasks : Signal (Task.Task Effects.Never ())
port tasks =
  app.tasks
