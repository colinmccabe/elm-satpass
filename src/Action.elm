module Action where

import Model exposing (..)


type Action
    = Passes (List Pass)
    | SatFilter (Maybe String)
    | MinEl Int
    | StartHour Int
    | EndHour Int
    | NoOp
