module Types where

import Date exposing (Date)


type alias SatName =
    String


type alias Deg =
    Int


type alias Pass =
    { satName : SatName
    , maxEl : Deg
    , startTime : Date
    , apogeeTime : Date
    , endTime : Date
    , startAz : Deg
    , endAz : Deg
    }


type alias Tle =
    { satName : SatName
    , line1 : String
    , line2 : String
    }
