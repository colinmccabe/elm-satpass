module Satellite (..) where

import Dict exposing (Dict)
import Json.Decode as JD exposing ((:=))
import Json.Encode as JE
import Native.Satellite
import String
import Task exposing (Task)
import Time exposing (Time)


type alias SatName =
  String


type alias PassId =
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


type alias Pass =
  { passId : PassId
  , satName : SatName
  , maxEl : Deg
  , startTime : Time
  , apogeeTime : Time
  , endTime : Time
  , startAz : Deg
  , endAz : Deg
  }


type alias LookAngle =
  { elevation : Deg
  , azimuth : Deg
  }


getPasses : Coords -> Dict String Tle -> Time -> Time -> Task a (Result String (List Pass))
getPasses coords tles begin duration =
  Native.Satellite.getPasses
    (encCoords coords)
    (JE.list (encTleList tles))
    (JE.float begin)
    (JE.float duration)
    |> Task.map (JD.decodeValue (JD.list decodePass))


getLookAngle : Coords -> Tle -> Time -> Task a (Result String LookAngle)
getLookAngle coords tle time =
  Native.Satellite.getLookAngle (encCoords coords) (encTle tle) (JE.float time)
    |> Task.map (JD.decodeValue decodeLookAngle)


encCoords : Coords -> JE.Value
encCoords coords =
  JE.object
    [ ( "latitude", JE.float coords.latitude )
    , ( "longitude", JE.float coords.longitude )
    ]


encTleList : Dict String Tle -> List JE.Value
encTleList tles =
  List.map
    (\( satName, tle ) ->
      JE.object
        [ ( "satName", JE.string satName )
        , ( "tle", encTle tle )
        ]
    )
    (Dict.toList tles)


encTle : Tle -> JE.Value
encTle tle =
  JE.object
    [ ( "line1", JE.string tle.line1 )
    , ( "line2", JE.string tle.line2 )
    ]


decodePass : JD.Decoder Pass
decodePass =
  JD.object8
    (\passId satName maxEl startTime apogeeTime endTime startAz endAz ->
      { passId = passId
      , satName = satName
      , maxEl = maxEl
      , startTime = startTime
      , apogeeTime = apogeeTime
      , endTime = endTime
      , startAz = startAz
      , endAz = endAz
      }
    )
    ("passId" := JD.string)
    ("satName" := JD.string)
    ("maxEl" := JD.float)
    ("startTime" := JD.float)
    ("apogeeTime" := JD.float)
    ("endTime" := JD.float)
    ("startAz" := JD.float)
    ("endAz" := JD.float)


decodeLookAngle : JD.Decoder LookAngle
decodeLookAngle =
  JD.object2
    (\elevation azimuth ->
      { elevation = elevation
      , azimuth = azimuth
      }
    )
    ("elevation" := JD.float)
    ("azimuth" := JD.float)


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
