port module LookAngleTable
    exposing
        ( Model
        , init
        , Msg
        , update
        , view
        , subs
        )

import Date
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Platform.Cmd exposing (Cmd)
import String
import Time exposing (Time)
import Types exposing (..)


type alias LookAngle =
    { passId : PassId
    , elevation : Deg
    , azimuth : Deg
    , dopplerFactor : Float
    }


type alias ParentModel a =
    { a
        | coords : Coords
        , tles : Dict String Tle
        , passes : Dict PassId Pass
    }


type alias Model =
    Dict PassId LookAngle


init : Model
init =
    Dict.empty


type Msg
    = Tick Time
    | LookAngles (List LookAngle)



-- Cmds


type alias LookAngleReq =
    { time : Time
    , coords : Coords
    , sats : List { passId : PassId, tle : Tle }
    }


port sendLookAngleReq : LookAngleReq -> Cmd msg


nextReq : ParentModel r -> Time -> LookAngleReq
nextReq { coords, tles, passes } time =
    let
        idTleRecord pass =
            Dict.get pass.satName tles
                |> Maybe.map (\tle -> { passId = pass.passId, tle = tle })
    in
        passes
            |> Dict.toList
            |> List.map snd
            |> List.filter (\pass -> time > pass.startTime && time < pass.endTime)
            |> List.filterMap idTleRecord
            |> (\sats -> { time = time, coords = coords, sats = sats })



-- Subs


port recvLookAngles : (List LookAngle -> msg) -> Sub msg


subs : Model -> Sub Msg
subs model =
    Sub.batch
        [ Time.every Time.second Tick
        , recvLookAngles LookAngles
        ]



-- Update


update : ParentModel a -> Msg -> Model -> ( Model, Cmd Msg )
update parentModel action model =
    case action of
        Tick time ->
            ( model
            , sendLookAngleReq (nextReq parentModel time)
            )

        LookAngles lookAngles ->
            let
                newModel =
                    lookAngles
                        |> List.map (\a -> ( a.passId, a ))
                        |> Dict.fromList
            in
                ( newModel
                , Cmd.none
                )



-- View


view : Time -> Dict PassId Pass -> Model -> Html a
view time passes lookAngles =
    let
        passAnglePairs =
            Dict.merge (\_ _ l -> l)
                (\_ lookAngle pass l -> l ++ [ ( lookAngle, pass ) ])
                (\_ _ l -> l)
                lookAngles
                passes
                []
    in
        case passAnglePairs of
            [] ->
                div [] []

            _ ->
                div []
                    [ h3 [ style [ ( "text-align", "center" ) ] ]
                        [ Html.text "Passing now" ]
                    , table
                        [ class "table"
                        , style [ ( "text-align", "center" ) ]
                        ]
                        [ tableHead
                        , tbody [] (List.map (passRow time) passAnglePairs)
                        ]
                    ]


tableHead : Html a
tableHead =
    let
        th' txt =
            th [ style [ ( "text-align", "center" ) ] ]
                [ text txt ]
    in
        thead []
            [ tr []
                [ th' "Satellite"
                , th' "El"
                , th' "Start"
                , th' "Apogee"
                , th' "End"
                , th' "Start Az"
                , th' "Az"
                , th' "End Az"
                ]
            ]


passRow : Time -> ( LookAngle, Pass ) -> Html a
passRow time ( lookAngle, pass ) =
    let
        td' str =
            td [] [ (text str) ]

        showDegrees deg =
            deg |> ceiling |> toString |> \s -> s ++ "°"

        showTime time =
            let
                h =
                    time |> Date.fromTime |> Date.hour |> toString

                mm =
                    time
                        |> Date.fromTime
                        |> Date.minute
                        |> toString
                        |> String.padLeft 2 '0'
            in
                h ++ ":" ++ mm

        risingSettingArrow =
            if time <= pass.apogeeTime then
                "↑"
            else
                "↓"

        elText =
            showDegrees lookAngle.elevation
                ++ " ("
                ++ showDegrees pass.maxEl
                ++ ") "
                ++ risingSettingArrow

        rowClass =
            "success"
    in
        tr [ class rowClass ]
            [ td [] [ strong [] [ text pass.satName ] ]
            , td' elText
            , td' (showTime pass.startTime)
            , td' (showTime pass.apogeeTime)
            , td' (showTime pass.endTime)
            , td' (showDegrees pass.startAz)
            , td' (showDegrees lookAngle.azimuth)
            , td' (showDegrees pass.endAz)
            ]
