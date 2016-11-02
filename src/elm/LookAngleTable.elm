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
import Http
import Platform.Cmd exposing (Cmd)
import String
import Task
import Json.Decode as JD exposing ((:=))
import Time exposing (Time)
import Types exposing (..)


type alias LookAngle =
    { elevation : Float
    , azimuth : Float
    }


type alias Model =
    Result String (Dict SatName LookAngle)


init : Model
init =
    Err "Loading..."


type Msg
    = Tick Time
    | LookAngles (Dict String LookAngle)
    | Fail String



-- Cmds


decodePos : JD.Decoder LookAngle
decodePos =
    JD.object2 LookAngle
        ("elevation" := JD.float)
        ("azimuth" := JD.float)


satList : Dict PassId Pass -> Time -> Dict SatName PassId
satList passes time =
    passes
        |> Dict.toList
        |> List.map snd
        |> List.filter (\pass -> time > pass.startTime && time < pass.endTime)
        |> List.map (\pass -> ( pass.satName, pass.passId ))
        |> Dict.fromList


getLookAngles : Dict SatName PassId -> Cmd Msg
getLookAngles sats =
    if Dict.isEmpty sats then
        Task.perform Fail LookAngles (Task.succeed Dict.empty)
    else
        let
            satNames =
                Dict.keys sats

            queryParams =
                [ ( "sats", String.join "," satNames ) ]

            url =
                Http.url "http://colin-tower:8080/satpass/pos" queryParams

            mergeFunc =
                \lookAngles ->
                    Dict.merge (\key passId acc -> Debug.crash "extra passId entry")
                        (\key passId lookAngle acc -> ( passId, lookAngle ) :: acc)
                        (\key lookAngle acc -> Debug.crash "extra lookAngle entry")
                        sats
                        lookAngles
                        []

            httpTask =
                Http.get (JD.dict decodePos) url
                    |> Task.map mergeFunc
                    |> Task.map Dict.fromList
                    |> Task.mapError toString
        in
            Task.perform Fail LookAngles httpTask



-- Subs


subs : Model -> Sub Msg
subs model =
    Time.every Time.second Tick



-- Update


update : Dict PassId Pass -> Msg -> Model -> ( Model, Cmd Msg )
update passes action model =
    case action of
        Tick time ->
            ( model
            , getLookAngles (satList passes time)
            )

        LookAngles lookAngles ->
            ( Ok lookAngles
            , Cmd.none
            )

        Fail msg ->
            ( model, Cmd.none )



-- View


view : Time -> Dict PassId Pass -> Model -> Html a
view time passes lookAnglesRes =
    case lookAnglesRes of
        Ok lookAngles ->
            angleTable time passes lookAngles

        Err msg ->
            p [ style [ ( "text-align", "center" ) ] ]
                [ text msg ]


angleTable : Time -> Dict PassId Pass -> Dict PassId LookAngle -> Html a
angleTable time passes lookAngles =
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
                p [ style [ ( "text-align", "center" ) ] ]
                    [ text "None" ]

            _ ->
                div []
                    [ table
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
        showDegrees deg =
            deg |> toString |> \s -> s ++ "°"

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
            showDegrees (ceiling lookAngle.elevation)
                ++ " ("
                ++ showDegrees pass.maxEl
                ++ ") "
                ++ risingSettingArrow

        rowClass =
            "success"

        td' str =
            td [] [ (text str) ]
    in
        tr [ class rowClass ]
            [ td [] [ strong [] [ text pass.satName ] ]
            , td' elText
            , td' (showTime pass.startTime)
            , td' (showTime pass.apogeeTime)
            , td' (showTime pass.endTime)
            , td' (showDegrees pass.startAz)
            , td' (showDegrees (ceiling lookAngle.azimuth))
            , td' (showDegrees pass.endAz)
            ]
