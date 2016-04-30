module SatPass (Model, init, Action(Init, LookAngleTable, UpdateTime), update, view) where

import Dict exposing (Dict)
import Effects exposing (Effects)
import PassFilter
import Html as H exposing (Html)
import Html.Attributes as HA
import Html.Events
import Http
import LookAngleTable
import PassTable
import Satellite exposing (..)
import Signal
import Task exposing (Task)
import Time exposing (Time)


duration : Time
duration =
  16 * Time.hour


sats : List SatName
sats =
  [ "AO-85"
  , "CO-55"
  , "CO-57"
  , "CO-58"
  , "CO-65"
  , "CO-66"
  , "FO-29"
  , "ISS"
  , "LILACSAT-2"
  , "NO-44"
  , "NO-84"
  , "SO-50"
  , "UKUBE-1"
  , "XW-2A"
  , "XW-2B"
  , "XW-2C"
  , "XW-2D"
  , "XW-2F"
  ]


type alias Model =
  { status : String
  , coords : Coords
  , time : Time
  , tles : Dict SatName Tle
  , passes : List Pass
  , loadedUpTo : Time
  , filter : PassFilter.Model
  , lookAngleTable : LookAngleTable.Model
  }


init : ( Model, Effects Action )
init =
  ( { status = "Trying to get location..."
    , coords = { latitude = 0.0, longitude = 0.0, altitude = 0.0 }
    , time = 0.0
    , tles = Dict.empty
    , passes = []
    , loadedUpTo = 0.0
    , filter = PassFilter.init
    , lookAngleTable = LookAngleTable.init
    }
  , Effects.none
  )


type Action
  = Init ( Time, Coords )
  | Tle (Result String (Dict SatName Tle))
  | Passes ( Time, List Pass )
  | PassesFail String
  | Filter PassFilter.Action
  | UpdateTime Time
  | LookAngleTable LookAngleTable.Action
  | LoadMorePasses
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
      , getTles sats
      )

    Tle (Ok tles) ->
      let
        model' =
          { model | tles = tles, status = "Trying to get passes..." }

        begin =
          model'.time - 1 * Time.hour
      in
        ( model'
        , getPasses model' begin (begin + duration)
        )

    Tle (Err msg) ->
      ( { model | status = "Failed to get TLEs: " ++ msg }
      , Effects.none
      )

    Passes ( endTime, newPasses ) ->
      ( { model
          | passes = model.passes ++ newPasses
          , loadedUpTo = endTime
          , status = "No passes meet criteria"
        }
      , Effects.none
      )

    PassesFail _ ->
      ( { model | status = "Failed to get passes" }
      , Effects.none
      )

    Filter action ->
      ( { model | filter = PassFilter.update action model.filter }
      , Effects.none
      )

    UpdateTime time ->
      ( { model | time = time }
      , Effects.none
      )

    LookAngleTable childAction ->
      let
        ( lookAngleModel, lookAngleEffects ) =
          LookAngleTable.update model childAction model.lookAngleTable
      in
        ( { model | lookAngleTable = lookAngleModel }
        , Effects.map LookAngleTable lookAngleEffects
        )

    LoadMorePasses ->
      ( model
      , getPasses model model.loadedUpTo (model.loadedUpTo + duration)
      )

    NoOp ->
      ( model, Effects.none )


view : Signal.Address Action -> Model -> Html
view addr model =
  let
    passTable =
      case List.filter (PassFilter.pred model.filter) model.passes of
        [] ->
          H.div [] [ H.text model.status ]

        filteredPasses ->
          PassTable.view model.time filteredPasses
  in
    H.div
      [ HA.class "container" ]
      [ LookAngleTable.view model.time model.lookAngleTable
      , H.div [ HA.style [ ( "height", "5px" ) ] ] []
      , H.h3
          [ HA.style [ ( "text-align", "center" ) ] ]
          [ H.text "Future passes" ]
      , PassFilter.view
          (Signal.forwardTo addr Filter)
          sats
          model.filter
      , passTable
      , loadMoreButton addr
      ]


loadMoreButton : Signal.Address Action -> Html
loadMoreButton addr =
  H.div
    [ HA.style
        [ ( "text-align", "center" )
        , ( "margin-bottom", "20px" )
        ]
    ]
    [ H.button
        [ HA.class "btn btn-primary"
        , Html.Events.onClick addr LoadMorePasses
        ]
        [ H.text "Load more" ]
    ]



-- Effects


getTles : List SatName -> Effects Action
getTles sats =
  Http.getString "nasabare.txt"
    |> (flip Task.onError) (\_ -> Http.getString "https://s3.amazonaws.com/cmccabe/keps/nasabare.txt")
    |> Task.map (parseTle sats)
    |> Task.mapError toString
    |> Task.toResult
    |> Task.map Tle
    |> Effects.task


getPasses : Model -> Time -> Time -> Effects Action
getPasses { coords, tles } begin end =
  tles
    |> Dict.toList
    |> List.map
        (\( satName, tle ) ->
          Satellite.getPasses coords satName tle begin duration
        )
    |> Task.sequence
    |> Task.map List.concat
    |> Task.map (List.sortBy .startTime)
    |> Task.map (\passes -> ( end, passes ))
    |> Task.map Passes
    |> (flip Task.onError) (Task.succeed << PassesFail)
    |> Effects.task
