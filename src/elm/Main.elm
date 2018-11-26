port module Main exposing (main)

import Browser
import Debug
import Dict exposing (Dict)
import Html as H exposing (Html)
import Html.Attributes as HA
import Html.Events
import Html.Lazy
import Http
import Json.Decode as JD
import Json.Encode as JE
import LookAngleTable
import PassFilter
import PassTable
import Task exposing (Task)
import Time
import Types exposing (..)


main : Program () Model Msg
main =
    Browser.document
        { init = \_ -> init
        , update = update
        , view = view
        , subscriptions =
            \_ ->
                Sub.batch
                    [ Time.every 1000 Tick
                    , responses decodePortResp
                    ]
        }


type alias Constants =
    { history : Int
    , loadMoreInterval : Int
    , defaultLocation : Location
    , blacklist : List SatName
    }


constants : Constants
constants =
    { history = 10 * 60 * 1000
    , loadMoreInterval = 6 * 60 * 60 * 1000
    , defaultLocation =
        { latitude = 0.0
        , longitude = 0.0
        , altitude = 0
        }
    , blacklist =
        [ "Challenger"
        , "DUTHsat"
        , "GomX-3"
        , "OBJECT B"
        , "OBJECT C"
        , "OBJECT D"
        , "OBJECT E"
        , "OBJECT F"
        , "OBJECT J"
        , "OBJECT K"
        , "Pegasus-1"
        ]
    }


type alias Model =
    { msg : UserMsg
    , location : Location
    , timezone : Time.Zone
    , time : Time.Posix
    , tles : Dict SatName Tle
    , passes : Dict PassId Pass
    , lookAngles : Dict PassId LookAngle
    , loadedUpTo : Time.Posix
    , filter : PassFilter.Model
    }


init : ( Model, Cmd Msg )
init =
    ( { msg = Show Info "Trying to get location..."
      , location = constants.defaultLocation
      , timezone = Time.utc
      , time = Time.millisToPosix 0
      , tles = Dict.empty
      , passes = Dict.empty
      , lookAngles = Dict.empty
      , loadedUpTo = Time.millisToPosix 0
      , filter = PassFilter.init
      }
    , getTimestamp
    )


type Msg
    = InitTime Time.Zone Time.Posix
    | Geolocation (Maybe Location)
    | TleString String
    | LookAngles (List LookAngle)
    | Passes { end : Time.Posix, passes : List Pass }
    | Filter PassFilter.Msg
    | Tick Time.Posix
    | LoadMorePasses
    | Fail String



-- Cmds


getTimestamp : Cmd Msg
getTimestamp =
    Task.map2 InitTime Time.here Time.now
        |> Task.perform Basics.identity


getTles : Cmd Msg
getTles =
    Http.getString "nasabare.txt"
        |> Http.send
            (\res ->
                case res of
                    Ok body ->
                        TleString body

                    Err e ->
                        Fail (Debug.toString e)
            )


port requests : JE.Value -> Cmd msg


type PortReq
    = LookAnglesReq
        { time : Time.Posix
        , location : Location
        , sats : List { id : PassId, tle : Tle }
        }
    | PassesReq
        { location : Location
        , begin : Time.Posix
        , end : Time.Posix
        , sats : List { satName : SatName, tle : Tle }
        }


encodePortReq : PortReq -> JE.Value
encodePortReq req =
    case req of
        LookAnglesReq lr ->
            JE.object
                [ ( "type", JE.string "LookAnglesReq" )
                , ( "time", JE.int (Time.posixToMillis lr.time) )
                , ( "location", encodeLocation lr.location )
                , ( "sats"
                  , JE.list
                        (\sat -> JE.object [ ( "id", JE.string sat.id ), ( "tle", encodeTle sat.tle ) ])
                        lr.sats
                  )
                ]

        PassesReq pr ->
            JE.object
                [ ( "type", JE.string "PassesReq" )
                , ( "location", encodeLocation pr.location )
                , ( "begin", JE.int (Time.posixToMillis pr.begin) )
                , ( "end", JE.int (Time.posixToMillis pr.end) )
                , ( "sats"
                  , JE.list
                        (\sat -> JE.object [ ( "satName", JE.string sat.satName ), ( "tle", encodeTle sat.tle ) ])
                        pr.sats
                  )
                ]



-- Subs


port responses : (JD.Value -> msg) -> Sub msg


decodePortResp : JD.Value -> Msg
decodePortResp json =
    let
        decoder =
            JD.oneOf
                [ JD.field
                    "Geolocation"
                    (JD.maybe decodeLocation |> JD.map Geolocation)
                , JD.field
                    "LookAngles"
                    (JD.list decodeLookAngle |> JD.map LookAngles)
                , JD.field
                    "Passes"
                    (JD.map2
                        (\end passes -> Passes { end = end, passes = passes })
                        (JD.field "end" JD.int |> JD.map Time.millisToPosix)
                        (JD.field "passes" (JD.list decodePass))
                    )
                ]
    in
    case JD.decodeValue decoder json of
        Ok msg ->
            msg

        Err e ->
            Fail ("Error decoding port response: " ++ Debug.toString e)



-- Update


update : Msg -> Model -> ( Model, Cmd Msg )
update action model =
    case action of
        InitTime timezone time ->
            ( { model
                | timezone = timezone
                , time = time
                , loadedUpTo = Time.millisToPosix (Time.posixToMillis time - constants.history)
              }
            , Cmd.none
            )

        Geolocation location ->
            let
                newModel =
                    case location of
                        Just l ->
                            { model | location = l }

                        Nothing ->
                            model
            in
            ( newModel, getTles )

        TleString tleStr ->
            let
                tles =
                    parseTle constants.blacklist tleStr

                model_ =
                    { model
                        | tles = tles
                        , msg = Show Info "Getting passes..."
                    }
            in
            ( model_
            , requests (nextPassReq constants.loadMoreInterval model_)
            )

        Passes { end, passes } ->
            let
                newPassesDict =
                    passes
                        |> List.map (\pass -> ( pass.passId, pass ))
                        |> Dict.fromList
            in
            ( { model
                | passes = Dict.union model.passes newPassesDict
                , loadedUpTo = end
                , msg = Hide
              }
            , Cmd.none
            )

        LookAngles lookAnglesList ->
            let
                lookAngles =
                    lookAnglesList
                        |> List.map (\angle -> ( angle.id, angle ))
                        |> Dict.fromList
            in
            ( { model | lookAngles = lookAngles }
            , Cmd.none
            )

        Filter filterAction ->
            ( { model | filter = PassFilter.update filterAction model.filter }
            , Cmd.none
            )

        Tick time ->
            ( { model | time = time }
            , requests (nextLookAngleReq model time)
            )

        LoadMorePasses ->
            ( model
            , requests (nextPassReq constants.loadMoreInterval model)
            )

        Fail msg ->
            ( { model | msg = Show Error msg }
            , Cmd.none
            )


nextPassReq : Int -> Model -> JE.Value
nextPassReq interval { location, tles, loadedUpTo } =
    tles
        |> Dict.toList
        |> List.map
            (\( satName, tle ) ->
                { satName = satName
                , tle = tle
                }
            )
        |> (\sats ->
                PassesReq
                    { location = location
                    , begin = loadedUpTo
                    , end = Time.millisToPosix (Time.posixToMillis loadedUpTo + interval)
                    , sats = sats
                    }
           )
        |> encodePortReq


nextLookAngleReq : Model -> Time.Posix -> JE.Value
nextLookAngleReq { location, tles, passes } time =
    let
        timeMillis =
            Time.posixToMillis time

        idTleRecord pass =
            Dict.get pass.satName tles
                |> Maybe.map (\tle -> { id = pass.passId, tle = tle })
    in
    passes
        |> Dict.filter (\_ pass -> timeMillis > Time.posixToMillis pass.startTime && timeMillis < Time.posixToMillis pass.endTime)
        |> Dict.values
        |> List.filterMap idTleRecord
        |> (\sats ->
                LookAnglesReq
                    { time = time
                    , location = location
                    , sats = sats
                    }
           )
        |> encodePortReq



-- View


view : Model -> Browser.Document Msg
view model =
    let
        filteredPasses =
            model.passes
                |> Dict.filter (\_ p -> PassFilter.pred model.timezone model.filter p)
                |> Dict.values
    in
    { title = "SatPass"
    , body =
        [ H.div
            [ HA.class "container"
            , HA.style "max-width" "980px"
            ]
            [ locationBox model.location
            , infoBox model.msg
            , H.h3 [ HA.style "text-align" "center" ]
                [ H.text "Current passes" ]
            , LookAngleTable.view model.timezone model.time model.passes model.lookAngles
            , H.div [ HA.style "height" "5px" ] []
            , H.h3 [ HA.style "text-align" "center" ]
                [ H.text "Future passes" ]
            , H.map Filter (PassFilter.view model.filter)
            , PassTable.view model.timezone model.time filteredPasses
            , loadMoreButton
            ]
        ]
    }


locationBox : Location -> Html a
locationBox l =
    H.div
        [ HA.style "margin-top" "5px"
        , HA.style "text-align" "right"
        ]
        [ H.text
            ("Lat: "
                ++ String.fromFloat l.latitude
                ++ " | Lon: "
                ++ String.fromFloat l.longitude
                ++ " | Alt: "
                ++ String.fromFloat l.altitude
            )
        ]


infoBox : UserMsg -> Html a
infoBox userMsg =
    case userMsg of
        Hide ->
            H.div [] []

        Show level str ->
            let
                cssClass =
                    case level of
                        Info ->
                            "bg-info"

                        Warning ->
                            "bg-warning"

                        Error ->
                            "bg-error"
            in
            H.p
                [ HA.class cssClass
                , HA.style "padding" "10px"
                , HA.style "margin-top" "10px"
                , HA.style "text-align" "center"
                , HA.style "font-weight" "bold"
                ]
                [ H.text str ]


loadMoreButton : Html Msg
loadMoreButton =
    H.div
        [ HA.style "text-align" "center"
        , HA.style "margin-bottom" "20px"
        ]
        [ H.button
            [ HA.class "btn btn-primary"
            , Html.Events.onClick LoadMorePasses
            ]
            [ H.text "Load more" ]
        ]
