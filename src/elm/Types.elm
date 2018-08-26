module Types exposing (..)

import Debug
import Dict exposing (Dict)
import Time


type alias Timestamp =
    Int


type UserMsg
    = Show Level String
    | Hide


type Level
    = Info
    | Warning
    | Error


type alias Location =
    { latitude : Float
    , longitude : Float
    , altitude : Float
    }

type alias SatName =
    String


type alias PassId =
    String


type alias Deg =
    Float


type alias Tle =
    { line1 : String
    , line2 : String
    }


type alias Pass =
    { passId : PassId
    , satName : SatName
    , maxEl : Deg
    , startTime : Timestamp
    , apogeeTime : Timestamp
    , endTime : Timestamp
    , startAz : Deg
    , endAz : Deg
    }


type alias LookAngle =
    { id : PassId
    , elevation : Deg
    , azimuth : Deg
    , dopplerFactor : Float
    }


parseTle : List SatName -> String -> Dict SatName Tle
parseTle sats rawTle =
    rawTle
        |> String.split "\n"
        |> groupTleLines sats
        |> Dict.fromList


groupTleLines : List SatName -> List String -> List ( SatName, Tle )
groupTleLines blacklist lines =
    case lines of
        satName :: tle1 :: tle2 :: rest ->
            if not (List.member satName blacklist) then
                ( satName, { line1 = tle1, line2 = tle2 } ) :: groupTleLines blacklist rest
            else
                groupTleLines blacklist rest

        _ ->
            []


showTime : Timestamp -> String
showTime time =
    let
        posixTime =
            Time.millisToPosix time

        h =
            posixTime |> Time.toHour Time.utc |> String.fromInt

        mm =
            posixTime
                |> Time.toMinute Time.utc
                |> String.fromInt
                |> String.padLeft 2 '0'
    in
        h ++ ":" ++ mm
