port module LookAngleTable
    exposing
        ( Model
        , init
        , Msg
        , update
        , view
        , subs
        )

import Dict exposing (Dict)
import Geolocation
import Html exposing (..)
import Html.Attributes exposing (..)
import Platform.Cmd exposing (Cmd)
import Time exposing (Time)
import Types exposing (..)


type alias LookAngle =
    { id : PassId
    , elevation : Deg
    , azimuth : Deg
    , dopplerFactor : Float
    }


type alias Context a =
    { a
        | location : Geolocation.Location
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
    , latitude : Float
    , longitude : Float
    , altitude : Float
    , sats : List { id : PassId, tle : Tle }
    }


port getLookAngles : LookAngleReq -> Cmd msg



-- Subs


port recvLookAngles : (List LookAngle -> msg) -> Sub msg


subs : Model -> Sub Msg
subs model =
    Sub.batch
        [ Time.every Time.second Tick
        , recvLookAngles LookAngles
        ]



-- Update


update : Context a -> Msg -> Model -> ( Model, Cmd Msg )
update context action model =
    case action of
        Tick time ->
            ( model
            , getLookAngles (nextReq context time)
            )

        LookAngles lookAngles ->
            let
                newModel =
                    lookAngles
                        |> List.map (\angle -> ( angle.id, angle ))
                        |> Dict.fromList
            in
                ( newModel
                , Cmd.none
                )


nextReq : Context a -> Time -> LookAngleReq
nextReq { location, tles, passes } time =
    let
        idTleRecord pass =
            Dict.get pass.satName tles
                |> Maybe.map (\tle -> { id = pass.passId, tle = tle })
    in
        passes
            |> Dict.filter (\_ pass -> time > pass.startTime && time < pass.endTime)
            |> Dict.values
            |> List.filterMap idTleRecord
            |> (\sats ->
                    { time = time
                    , latitude = location.latitude
                    , longitude = location.longitude
                    , altitude =
                        location.altitude
                            |> Maybe.map .value
                            |> Maybe.withDefault 0.0
                    , sats = sats
                    }
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
        th_ txt =
            th [ style [ ( "text-align", "center" ) ] ]
                [ text txt ]
    in
        thead []
            [ tr []
                [ th_ "Satellite"
                , th_ "El"
                , th_ "Start"
                , th_ "Apogee"
                , th_ "End"
                , th_ "Start Az"
                , th_ "Az"
                , th_ "End Az"
                ]
            ]


passRow : Time -> ( LookAngle, Pass ) -> Html a
passRow time ( lookAngle, pass ) =
    let
        td_ str =
            td [] [ (text str) ]

        showDegrees deg =
            deg |> ceiling |> toString |> \s -> s ++ "°"

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
            , td_ elText
            , td_ (showTime pass.startTime)
            , td_ (showTime pass.apogeeTime)
            , td_ (showTime pass.endTime)
            , td_ (showDegrees pass.startAz)
            , td_ (showDegrees lookAngle.azimuth)
            , td_ (showDegrees pass.endAz)
            ]
