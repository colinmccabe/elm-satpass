module Types (..) where

import Dict exposing (Dict)
import String
import Time exposing (Time)


type alias SatName =
  String


type alias Deg =
  Float


type alias Coords =
  { latitude : Deg
  , longitude : Deg
  }


type alias Tle =
  { line1 : String
  , line2 : String
  }


type alias PassReq =
  { coords : Coords
  , begin : Time
  , duration : Time
  , tles : List ( SatName, Tle )
  }


type alias Pass =
  { uid : String
  , satName : SatName
  , maxEl : Deg
  , startTime : Time
  , apogeeTime : Time
  , endTime : Time
  , startAz : Deg
  , endAz : Deg
  }


type alias LookAngleReq =
  { coords : Coords
  , time : Time
  , tles : List ( SatName, Tle )
  }


type alias LookAngle =
  { elevation : Deg
  , azimuth : Deg
  }


parseTle : String -> Dict SatName Tle
parseTle rawTle =
  rawTle
    |> String.split "\n"
    |> groupTleLines
    |> Dict.fromList


groupTleLines : List String -> List ( SatName, Tle )
groupTleLines lines =
  case lines of
    satName :: tle1 :: tle2 :: rest ->
      ( satName, { line1 = tle1, line2 = tle2 } ) :: groupTleLines rest

    _ ->
      []
