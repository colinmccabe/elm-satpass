module PassPredictor (Pass, getPasses) where

import Date exposing (Date)
import Native.PassPredictor
import String
import Task exposing (Task)
import Time exposing (Time)


type alias Pass =
    { satName : String
    , maxEl : Int
    , startTime : Date
    , apogeeTime : Date
    , endTime : Date
    , startAz : Int
    , endAz : Int
    }


type alias Tle =
    { line1 : String
    , line2 : String
    }


getPasses : List String -> Time -> Time -> String -> Task String (List Pass)
getPasses desiredSats from duration rawTle =
    let toDate jsTime =
            Date.fromTime (jsTime * Time.millisecond)
        toElmPass nativePass =
            { nativePass
                | startTime = toDate nativePass.startTime
                , endTime = toDate nativePass.endTime
                , apogeeTime = toDate nativePass.apogeeTime
            }
        nativePasses =
            Native.PassPredictor.getPasses
                ( Time.inMilliseconds from )
                ( Time.inMilliseconds duration )
                ( parseTle rawTle desiredSats )
    in
        nativePasses
            |> Task.map (List.map toElmPass)


parseTle : String -> List String -> List (String, Tle)
parseTle rawTle desiredSats =
    rawTle
        |> String.split "\n"
        |> toAssocList
        |> List.filter (\(satName, _) -> List.member satName desiredSats)


toAssocList : List String -> List (String, Tle)
toAssocList list =
    case list of
        satName :: tle1 :: tle2 :: rest ->
            (satName, {line1 = tle1, line2 = tle2}) :: toAssocList rest
        _ ->
            []
