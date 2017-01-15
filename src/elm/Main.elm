port module Main exposing (main)

import Dict exposing (Dict)
import Geolocation exposing (Location)
import Html as H exposing (Html)
import Html.Attributes as HA
import Html.Events
import Html.Lazy
import Http
import LookAngleTable
import PassFilter
import PassTable
import Result.Extra
import Task exposing (Task)
import Time exposing (Time)
import Types exposing (..)


main : Program Never Model Msg
main =
    H.program
        { init = init
        , update = update
        , view = Html.Lazy.lazy view
        , subscriptions =
            (\_ ->
                Sub.batch
                    [ recvPasses Passes
                    , Time.every Time.second Tick
                    , recvLookAngles LookAngles
                    ]
            )
        }


type alias Constants =
    { history : Time
    , loadMoreInterval : Time
    , defaultLocation : Location
    , blacklist : List SatName
    }


constants : Constants
constants =
    { history = 30 * Time.minute
    , loadMoreInterval = 6 * Time.hour
    , defaultLocation =
        { latitude = 0.0
        , longitude = 0.0
        , accuracy = 1.0
        , altitude = Nothing
        , movement = Nothing
        , timestamp = 0.0
        }
    , blacklist =
        [ "GomX-3"
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
    { geoMsg : UserMsg
    , msg : UserMsg
    , location : Location
    , time : Time
    , tles : Dict SatName Tle
    , passes : Dict PassId Pass
    , lookAngles : Dict PassId LookAngle
    , loadedUpTo : Time
    , filter : PassFilter.Model
    }


init : ( Model, Cmd Msg )
init =
    ( { geoMsg = Hide
      , msg = Show Info "Trying to get location..."
      , location = constants.defaultLocation
      , time = 0.0
      , tles = Dict.empty
      , passes = Dict.empty
      , lookAngles = Dict.empty
      , loadedUpTo = 0.0
      , filter = PassFilter.init
      }
    , getTimestamp
    )


type Msg
    = Timestamp Time
    | Location (Result Geolocation.Error Location)
    | TleString String
    | Passes ( Time, List Pass )
    | LookAngles (List LookAngle)
    | Filter PassFilter.Msg
    | Tick Time
    | LoadMorePasses
    | Fail String



-- Cmds


getTimestamp : Cmd Msg
getTimestamp =
    Time.now |> Task.perform Timestamp


getLocation : Cmd Msg
getLocation =
    Geolocation.nowWith
        { enableHighAccuracy = False
        , timeout = Just 10000
        , maximumAge = Just (48 * 3600000)
        }
        |> Task.attempt Location


getTles : Cmd Msg
getTles =
    Http.getString "nasabare.txt"
        |> Http.send (Result.Extra.unpack (toString >> Fail) TleString)


type alias PassReq =
    { latitude : Float
    , longitude : Float
    , altitude : Float
    , begin : Time
    , end : Time
    , sats : List { satName : SatName, tle : Tle }
    }


port getPasses : PassReq -> Cmd msg


type alias LookAngleReq =
    { time : Time
    , latitude : Float
    , longitude : Float
    , altitude : Float
    , sats : List { id : PassId, tle : Tle }
    }


port getLookAngles : LookAngleReq -> Cmd msg



-- Subs


port recvPasses : (( Time, List Pass ) -> msg) -> Sub msg


port recvLookAngles : (List LookAngle -> msg) -> Sub msg



-- Update


update : Msg -> Model -> ( Model, Cmd Msg )
update action model =
    case action of
        Timestamp time ->
            ( { model
                | time = time
                , loadedUpTo = time - constants.history
              }
            , getLocation
            )

        Location (Ok location) ->
            ( { model
                | location = location
                , geoMsg = Hide
                , msg = Show Info "Getting TLEs..."
              }
            , getTles
            )

        Location (Err _) ->
            ( { model
                | geoMsg = Show Warning "Warning: Geolocation failed, fell back to 0°N, 0°E"
                , msg = Show Info "Getting TLEs..."
              }
            , getTles
            )

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
                , getPasses (nextPassReq constants.loadMoreInterval model_)
                )

        Passes ( endTime, newPasses ) ->
            let
                newPassesDict =
                    newPasses
                        |> List.map (\pass -> ( pass.passId, pass ))
                        |> Dict.fromList
            in
                ( { model
                    | passes = Dict.union model.passes newPassesDict
                    , loadedUpTo = endTime
                    , msg = Hide
                  }
                , Cmd.none
                )

        Filter action ->
            ( { model | filter = PassFilter.update action model.filter }
            , Cmd.none
            )

        Tick time ->
            ( { model | time = time }
            , getLookAngles (nextLookAngleReq model time)
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

        LoadMorePasses ->
            ( model
            , getPasses (nextPassReq constants.loadMoreInterval model)
            )

        Fail msg ->
            ( { model | msg = Show Error msg }
            , Cmd.none
            )


nextPassReq : Time -> Model -> PassReq
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
                { latitude = location.latitude
                , longitude = location.longitude
                , altitude =
                    location.altitude
                        |> Maybe.map .value
                        |> Maybe.withDefault 0.0
                , begin = loadedUpTo
                , end = loadedUpTo + interval
                , sats = sats
                }
           )


nextLookAngleReq : Model -> Time -> LookAngleReq
nextLookAngleReq { location, tles, passes } time =
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


view : Model -> Html Msg
view model =
    let
        filteredPasses =
            model.passes
                |> Dict.filter (\_ p -> PassFilter.pred model.filter p)
                |> Dict.values
    in
        H.div
            [ HA.class "container"
            , HA.style [ ( "max-width", "980px" ) ]
            ]
            [ infoBox model.geoMsg
            , infoBox model.msg
            , H.h3 [ HA.style [ ( "text-align", "center" ) ] ]
                [ H.text "Current passes" ]
            , LookAngleTable.view model.time model.passes model.lookAngles
            , H.div [ HA.style [ ( "height", "5px" ) ] ] []
            , H.h3 [ HA.style [ ( "text-align", "center" ) ] ]
                [ H.text "Future passes" ]
            , H.map Filter (PassFilter.view model.filter)
            , PassTable.view model.time filteredPasses
            , loadMoreButton
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
                    , HA.style
                        [ ( "padding", "10px" )
                        , ( "margin-top", "10px" )
                        , ( "text-align", "center" )
                        , ( "font-weight", "bold" )
                        ]
                    ]
                    [ H.text str ]


loadMoreButton : Html Msg
loadMoreButton =
    H.div
        [ HA.style
            [ ( "text-align", "center" )
            , ( "margin-bottom", "20px" )
            ]
        ]
        [ H.button
            [ HA.class "btn btn-primary"
            , Html.Events.onClick LoadMorePasses
            ]
            [ H.text "Load more" ]
        ]
