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
    , endTime : Date
    , startAz : Int
    , endAz : Int
    }


type alias Tle =
    { line1 : String
    , line2 : String
    }


getPasses : String -> List String -> Time -> Time -> Task a (Result String (List Pass))
getPasses rawTle desiredSats from duration =
    let toElmPass nativePass =
            { nativePass
                | startTime = Date.fromTime (nativePass.startTime * Time.millisecond)
                , endTime = Date.fromTime (nativePass.endTime * Time.millisecond)
            }
        nativePasses =
            Native.PassPredictor.getPasses
                ( parseTle rawTle desiredSats )
                ( Time.inMilliseconds from )
                ( Time.inMilliseconds duration )
    in
        nativePasses
            |> Task.map (List.map toElmPass)
            |> Task.toResult


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
