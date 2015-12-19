module Action where

import Model exposing (..)


type Action
    = Passes (List Pass)
    | FilterSat (Maybe String)
    | FilterMinEl Int
    | FilterStartHour Int
    | FilterEndHour Int
    | FilterReset
    | NoOp
