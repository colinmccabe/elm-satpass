module Types exposing (..)

import Debug
import Dict exposing (Dict)
import Json.Decode as JD
import Json.Encode as JE
import Time


type UserMsg
    = Show Level String
    | Hide


type Level
    = Info
    | Warning
    | Error


type alias Location =
    { latitude : Float
    , longitude : Float
    , altitude : Float
    }


encodeLocation : Location -> JE.Value
encodeLocation l =
    JE.object
        [ ( "latitude", JE.float l.latitude )
        , ( "longitude", JE.float l.longitude )
        , ( "altitude", JE.float l.altitude )
        ]


decodeLocation : JD.Decoder Location
decodeLocation =
    JD.map3 (\lat long alt -> { latitude = lat, longitude = long, altitude = alt })
        (JD.field "latitude" JD.float)
        (JD.field "longitude" JD.float)
        (JD.field "altitude" (JD.maybe JD.float |> JD.map (Maybe.withDefault 0)))


type alias SatName =
    String


type alias PassId =
    String


type alias Deg =
    Float


type alias Tle =
    { line1 : String
    , line2 : String
    }


encodeTle : Tle -> JE.Value
encodeTle tle =
    JE.object
        [ ( "line1", JE.string tle.line1 )
        , ( "line2", JE.string tle.line2 )
        ]


type alias Pass =
    { passId : PassId
    , satName : SatName
    , maxEl : Deg
    , startTime : Time.Posix
    , apogeeTime : Time.Posix
    , endTime : Time.Posix
    , startAz : Deg
    , endAz : Deg
    }


decodePass : JD.Decoder Pass
decodePass =
    JD.map8 Pass
        (JD.field "passId" JD.string)
        (JD.field "satName" JD.string)
        (JD.field "maxEl" JD.float)
        (JD.field "startTime" JD.int |> JD.map Time.millisToPosix)
        (JD.field "apogeeTime" JD.int |> JD.map Time.millisToPosix)
        (JD.field "endTime" JD.int |> JD.map Time.millisToPosix)
        (JD.field "startAz" JD.float)
        (JD.field "endAz" JD.float)


type alias LookAngle =
    { id : PassId
    , elevation : Deg
    , azimuth : Deg
    , dopplerFactor : Float
    }


decodeLookAngle : JD.Decoder LookAngle
decodeLookAngle =
    JD.map4 LookAngle
        (JD.field "id" JD.string)
        (JD.field "elevation" JD.float)
        (JD.field "azimuth" JD.float)
        (JD.field "dopplerFactor" JD.float)


decodeResult : JD.Decoder a -> JD.Decoder (Result String a)
decodeResult decoder =
    JD.oneOf
        [ JD.field "Ok" decoder |> JD.map Ok
        , JD.field "Err" JD.string |> JD.map Err
        ]


parseTle : List SatName -> String -> Dict SatName Tle
parseTle sats rawTle =
    rawTle
        |> String.split "\n"
        |> groupTleLines sats
        |> Dict.fromList


groupTleLines : List SatName -> List String -> List ( SatName, Tle )
groupTleLines blacklist lines =
    case lines of
        satName :: tle1 :: tle2 :: rest ->
            if not (List.member satName blacklist) then
                ( satName, { line1 = tle1, line2 = tle2 } ) :: groupTleLines blacklist rest

            else
                groupTleLines blacklist rest

        _ ->
            []


showTime : Time.Zone -> Time.Posix -> String
showTime timezone time =
    let
        h =
            time |> Time.toHour timezone |> String.fromInt

        mm =
            time
                |> Time.toMinute timezone
                |> String.fromInt
                |> String.padLeft 2 '0'
    in
    h ++ ":" ++ mm
