module Model where

import PassPredictor exposing (Pass)
import Set exposing (Set)
import Time exposing (Time)


type alias Model =
    { programStartTime : Maybe Time
    , passes : Result String (List Pass)
    , sats : Set String
    , satFilter : Maybe String
    , minEl : Int
    , startHour : Int
    , endHour : Int
    }
