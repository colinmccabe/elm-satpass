module SatPass (Model, init, Action(..), update, view) where

import Dict exposing (Dict)
import Effects exposing (Effects)
import PassFilter
import Html exposing (Html, div)
import Html.Attributes exposing (class)
import Http
import PassingTable
import PassTable
import Satellite exposing (..)
import Signal
import Task exposing (Task)
import Time exposing (Time)


duration : Time
duration =
  48 * Time.hour


sats : List String
sats =
  [ "FO-29"
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
  , "XW-2F"
  , "ISS"
  ]


type alias Model =
  { status : String
  , coords : Coords
  , time : Time
  , tles : Dict SatName Tle
  , passes : List Pass
  , filter : PassFilter.Model
  , lookAngles : List ( LookAngle, Pass )
  }


init : ( Model, Effects Action )
init =
  ( { status = "Trying to get location..."
    , coords = { latitude = 0.0, longitude = 0.0 }
    , time = 0.0
    , tles = Dict.empty
    , passes = []
    , filter = PassFilter.init
    , lookAngles = []
    }
  , Effects.none
  )


type Action
  = Init ( Time, Coords )
  | Tle (Result String (Dict SatName Tle))
  | Passes (Result String (List Pass))
  | Filter PassFilter.Action
  | Tick Time
  | LookAngles (List ( LookAngle, Pass ))
  | NoOp


update : Action -> Model -> ( Model, Effects Action )
update action model =
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
      let
        model' =
          { model | tles = tles, status = "Trying to get passes..." }

        begin =
          model'.time - 1 * Time.hour
      in
        ( model'
        , Effects.task
            (getPasses model' begin duration)
        )

    Tle (Err msg) ->
      ( { model | status = "Failed to get TLEs: " ++ msg }
      , Effects.none
      )

    Passes (Ok passes) ->
      ( { model | passes = passes }
      , Effects.none
      )

    Passes (Err _) ->
      ( { model | status = "Failed to get passes" }
      , Effects.none
      )

    Filter action ->
      ( { model | filter = PassFilter.update action model.filter }
      , Effects.none
      )

    Tick time ->
      let
        model' =
          { model | time = time }
      in
        ( model'
        , Effects.task (getLookAngles model')
        )

    LookAngles angles ->
      ( { model | lookAngles = angles }
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
          PassTable.view model.time filteredPasses
  in
    div
      [ class "container" ]
      [ PassFilter.view
          (Signal.forwardTo addr Filter)
          sats
          model.filter
      , PassingTable.view model.time model.lookAngles
      , Html.h4 [] [ Html.text "Future passes" ]
      , passTable
      ]



-- Tasks


getTles : List SatName -> Task a Action
getTles sats =
  Http.getString "nasabare.txt"
    |> (flip Task.onError) (\_ -> Http.getString "https://s3.amazonaws.com/cmccabe/keps/nasabare.txt")
    |> Task.map (parseTle sats)
    |> Task.mapError toString
    |> Task.toResult
    |> Task.map Tle


getPasses : Model -> Time -> Time -> Task a Action
getPasses { coords, tles } begin duration =
  Satellite.getPasses coords tles begin duration
    |> Task.map Passes


getLookAngles : Model -> Task a Action
getLookAngles { time, coords, tles, passes } =
  let
    getLookAngle pass tle =
      Satellite.getLookAngle coords tle time
        |> Task.map
            (\result ->
              case result of
                Ok lookAngle ->
                  Just ( lookAngle, pass )

                _ ->
                  Nothing
            )
  in
    passes
      |> List.filter (\pass -> time > pass.startTime && time < pass.endTime)
      |> List.filterMap
          (\pass -> Maybe.map (getLookAngle pass) (Dict.get pass.satName tles))
      |> Task.sequence
      |> Task.map (List.filterMap identity)
      |> Task.map LookAngles
