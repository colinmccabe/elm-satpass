module SatSelect (view) where

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (on)
import Json.Decode as JD
import Signal exposing (Address)
import Types exposing (SatName)


view : Address a -> (Maybe String -> a) -> List SatName -> Html
view addr action sats =
    let toOption sat =
            option
                [ value sat ]
                [ text sat ]
        optionValToAction val =
            if val == "Any" then
                action Nothing
            else
                action (Just val)
        decodeEvent =
            JD.customDecoder
                ( JD.at ["target", "value"] JD.string )
                ( optionValToAction >> Ok )
    in
        div
            [ class "form-group" ]
            [ label [] [ text "Sat" ]
            , select
                [ class "form-control"
                , on "change" decodeEvent (Signal.message addr)
                ]
                ( List.map toOption ("Any" :: sats))
            ]
