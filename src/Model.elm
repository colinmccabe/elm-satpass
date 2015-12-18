module Model where

import Date exposing (Date)
import Set exposing (Set)


type alias Model =
    { passes : List Pass
    , sats : Set String
    , satFilter : Maybe String
    , minEl : Int
    , startHour : Int
    , endHour : Int
    }


type alias PassRequest =
    { start : Float
    , duration : Float
    , tle : List (String, Tle)
    }


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
