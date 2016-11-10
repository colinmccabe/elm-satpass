module Types exposing (..)

import Date
import Http
import Time exposing (Time)


type alias SatName =
    String


type alias PassId =
    String


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


type alias LookAngle =
    { elevation : Float
    , azimuth : Float
    }


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


buildUrl : String -> List ( String, String ) -> String
buildUrl path params =
    params
        |> List.map (\( k, v ) -> Http.encodeUri k ++ "=" ++ Http.encodeUri v)
        |> String.join "&"
        |> \query -> path ++ "?" ++ query
