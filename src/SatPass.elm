module SatPass (Model, init, Action(..), update, view) where

import Dict exposing (Dict)
import Effects exposing (Effects)
import PassFilter
import Html exposing (Html, div)
import Html.Attributes exposing (class)
import Http
import PassTable
import Signal
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
  { status : String
  , coords : Coords
  , time : Time
  , tles : Dict SatName Tle
  , passes : List Pass
  , filter : PassFilter.Model
  , lookAngles : Dict SatName LookAngle
  }


init : ( Model, Effects Action )
init =
  ( { status = "Trying to get location..."
    , coords = { latitude = 0.0, longitude = 0.0 }
    , time = 0.0
    , tles = Dict.empty
    , passes = []
    , filter = PassFilter.init
    , lookAngles = Dict.empty
    }
  , Effects.none
  )


type Action
  = Init ( Time, Coords )
  | Tle (Result String (Dict SatName Tle))
  | Passes (List Pass)
  | Filter PassFilter.Action
  | Tick Time
  | LookAngles (List ( String, LookAngle ))
  | NoOp


type alias Mailboxes =
  { passesReq : Signal.Mailbox PassReq
  , lookAngleReq : Signal.Mailbox LookAngleReq
  }


update : Mailboxes -> Action -> Model -> ( Model, Effects Action )
update mailboxes action model =
  case action of
    Init ( time, coords ) ->
      ( { model
          | coords = coords
          , time = time
          , status = "Trying to get TLEs..."
        }
      , Effects.task (getTles sats)
      )

    Tle (Ok tles) ->
      ( { model | tles = tles, status = "Trying to get passes..." }
      , Effects.task
          (getPasses mailboxes.passesReq model.coords tles (model.time - 1 * Time.hour) duration)
      )

    Tle (Err msg) ->
      ( { model | status = "Failed to get TLEs: " ++ msg }
      , Effects.none
      )

    Passes passes ->
      ( { model | passes = passes }
      , Effects.none
      )

    Filter action ->
      ( { model | filter = PassFilter.update action model.filter }
      , Effects.none
      )

    Tick time ->
      let
        tlesOfSatsAboveHorizon =
          model.passes
            |> List.filter
                (\pass ->
                  time > pass.startTime && time < pass.endTime
                )
            |> List.filterMap
                (\pass ->
                  Dict.get pass.satName model.tles
                    |> Maybe.map (\tle -> ( pass.uid, tle ))
                )
      in
        ( { model | time = time }
        , Effects.task (getLookAngles mailboxes.lookAngleReq model.coords tlesOfSatsAboveHorizon time)
        )

    LookAngles angles ->
      ( { model | lookAngles = Dict.fromList angles }
      , Effects.none
      )

    NoOp ->
      ( model, Effects.none )


view : Signal.Address Action -> Model -> Html
view addr model =
  let
    passTable =
      case List.filter (PassFilter.pred model.filter) model.passes of
        [] ->
          div [] [ Html.text model.status ]

        filteredPasses ->
          PassTable.view model.time filteredPasses model.lookAngles
  in
    div
      [ class "container" ]
      [ PassFilter.view
          (Signal.forwardTo addr Filter)
          sats
          model.filter
      , passTable
      ]



-- Tasks


getTles : List SatName -> Task a Action
getTles sats =
  Http.getString "nasabare.txt"
    |> (flip Task.onError) (\_ -> Http.getString "https://s3.amazonaws.com/cmccabe/keps/nasabare.txt")
    |> Task.map parseTle
    |> Task.map (Dict.filter (\satName _ -> List.member satName sats))
    |> Task.mapError toString
    |> Task.toResult
    |> Task.map Tle


getPasses : Signal.Mailbox PassReq -> Coords -> Dict SatName Tle -> Time -> Time -> Task a Action
getPasses mailbox coords tles begin duration =
  let
    passReq =
      { coords = coords
      , begin = Time.inMilliseconds begin
      , duration = Time.inMilliseconds duration
      , tles = Dict.toList tles
      }
  in
    Signal.send mailbox.address passReq
      |> Task.toResult
      |> Task.map (\_ -> NoOp)


getLookAngles : Signal.Mailbox LookAngleReq -> Coords -> List ( SatName, Tle ) -> Time -> Task a Action
getLookAngles mailbox coords tles time =
  { coords = coords, time = time, tles = tles }
    |> Signal.send mailbox.address
    |> Task.toResult
    |> Task.map (\_ -> NoOp)