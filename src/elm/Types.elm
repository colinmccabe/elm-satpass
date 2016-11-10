module Types exposing (..)

import Date
import Dict exposing (Dict)
import String
import Time exposing (Time)


type UserMsg
    = Present Level String
    | Absent


type Level
    = Info
    | Warning
    | Error


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
    , startTime : Time
    , apogeeTime : Time
    , endTime : Time
    , startAz : Deg
    , endAz : Deg
    }


parseTle : List SatName -> String -> Dict SatName Tle
parseTle sats rawTle =
    rawTle
        |> String.split "\n"
        |> groupTleLines sats
        |> Dict.fromList


groupTleLines : List SatName -> List String -> List ( SatName, Tle )
groupTleLines sats lines =
    case lines of
        satName :: tle1 :: tle2 :: rest ->
            if List.member satName sats then
                ( satName, { line1 = tle1, line2 = tle2 } ) :: groupTleLines sats rest
            else
                groupTleLines sats rest

        _ ->
            []


showTime : Time -> String
showTime time =
    let
        h =
            time |> Date.fromTime |> Date.hour |> toString

        mm =
            time
                |> Date.fromTime
                |> Date.minute
                |> toString
                |> String.padLeft 2 '0'
    in
        h ++ ":" ++ mm
