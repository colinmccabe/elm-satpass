module Types exposing (..)

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
    , maxEl : Int
    , startTime : Time
    , apogeeTime : Time
    , endTime : Time
    , startAz : Int
    , endAz : Int
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
