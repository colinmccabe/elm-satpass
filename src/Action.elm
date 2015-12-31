module Action where

import PassPredictor exposing (Pass)
import Time exposing (Time)


type Action
    = Init Time
    | Passes (Result String (List Pass))
    | FilterSat (Maybe String)
    | FilterMinEl Int
    | FilterStartHour Int
    | FilterEndHour Int
    | FilterReset
