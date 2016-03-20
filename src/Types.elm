module Types (..) where

import String
import Time exposing (Time)


type alias SatName =
  String


type alias Deg =
  Int


type alias Coords =
  { latitude : Float
  , longitude : Float
  }


type alias Tle =
  { satName : SatName
  , line1 : String
  , line2 : String
  }


type alias PassReq =
  { coords : Coords
  , begin : Float
  , duration : Float
  , tles : List Tle
  }


type alias Pass =
  { satName : SatName
  , maxEl : Deg
  , startTime : Time
  , apogeeTime : Time
  , endTime : Time
  , startAz : Deg
  , endAz : Deg
  }


parseTle : String -> List Tle
parseTle rawTle =
  rawTle
    |> String.split "\n"
    |> groupTleLines


groupTleLines : List String -> List Tle
groupTleLines lines =
  case lines of
    satName :: tle1 :: tle2 :: rest ->
      { satName = satName, line1 = tle1, line2 = tle2 } :: groupTleLines rest

    _ ->
      []
