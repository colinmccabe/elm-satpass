module Model where

import PassPredictor exposing (Pass)
import Set exposing (Set)


type alias Model =
    { passes : Result String (List Pass)
    , sats : Set String
    , satFilter : Maybe String
    , minEl : Int
    , startHour : Int
    , endHour : Int
    }
