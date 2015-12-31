module Model where

import PassPredictor exposing (Pass)
import Set exposing (Set)
import Time exposing (Time)


type alias Model =
    { passes : Result String (List Pass)
    , satFilter : Maybe String
    , minEl : Int
    , startHour : Int
    , endHour : Int
    }
