module Slider (view) where

import Action exposing (Action)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (on)
import Json.Decode as JD
import Signal
import String

view
    : Signal.Address Action
    -> String
    -> (Int -> Action)
    -> Int 
    -> Int
    -> Int
    -> Int
    -> Html
view addr title action min max default currentVal =
    let decodeEvent =
            JD.customDecoder
                ( JD.at ["target", "value"] JD.string )
                ( String.toInt >> Result.map action )
    in
        div
            [ class "form-group" ]
            [ label [] [ text title ]
            , input
                [ type' "range"
                , Html.Attributes.min (toString min)
                , Html.Attributes.max (toString max)
                , step "1"
                , attribute "value" (toString default)
                , on "input" decodeEvent (Signal.message addr)
                ]
                []
            , text (toString currentVal)
            ]
