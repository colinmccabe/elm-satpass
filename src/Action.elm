module Action where

import Http
import PassPredictor exposing (Pass)
import Time exposing (Time)


type Action
    = Init Time
    | Tle (Result Http.Error String)
    | Passes (Result String (List Pass))
    | FilterSat (Maybe String)
    | FilterMinEl Int
    | FilterStartHour Int
    | FilterEndHour Int
    | FilterReset
