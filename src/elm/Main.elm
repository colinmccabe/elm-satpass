port module Main exposing (main)

import Dict exposing (Dict)
import Geolocation exposing (Location)
import Html as H exposing (Html)
import Html.App as App
import Html.Attributes as HA
import Html.Events
import Html.Lazy
import Http
import LookAngleTable
import PassFilter
import PassTable
import Task exposing (Task)
import Time exposing (Time)
import Types exposing (..)


main : Program Never
main =
    App.program
        { init = init context
        , update = update context
        , view = Html.Lazy.lazy (view context)
        , subscriptions = subs
        }


type alias Context =
    { history : Time
    , loadMoreInterval : Time
    , defaultLocation : Location
    , sats : List SatName
    }


context : Context
context =
    { history = 1 * Time.hour
    , loadMoreInterval = 16 * Time.hour
    , defaultLocation =
        { latitude = 0.0
        , longitude = 0.0
        , accuracy = 1.0
        , altitude = Nothing
        , movement = Nothing
        , timestamp = 0.0
        }
    , sats =
        [ "AO-85"
        , "CO-55"
        , "CO-57"
        , "CO-58"
        , "CO-65"
        , "CO-66"
        , "FO-29"
        , "ISS"
        , "LILACSAT-2"
        , "NO-44"
        , "NO-84"
        , "SO-50"
        , "UKUBE-1"
        , "XW-2A"
        , "XW-2B"
        , "XW-2C"
        , "XW-2D"
        , "XW-2F"
        ]
    }


type alias Model =
    { geoMsg : UserMsg
    , msg : UserMsg
    , location : Location
    , time : Time
    , tles : Dict SatName Tle
    , passes : Dict PassId Pass
    , loadedUpTo : Time
    , filter : PassFilter.Model
    , lookAngleTable : LookAngleTable.Model
    }


init : Context -> ( Model, Cmd Msg )
init context =
    ( { geoMsg = Absent
      , msg = Present Info "Trying to get location..."
      , location = context.defaultLocation
      , time = 0.0
      , tles = Dict.empty
      , passes = Dict.empty
      , loadedUpTo = 0.0
      , filter = PassFilter.init
      , lookAngleTable = LookAngleTable.init
      }
    , getTimestamp
    )


type Msg
    = Timestamp Time
    | Location (Maybe Location)
    | TleString String
    | Passes ( Time, List Pass )
    | Filter PassFilter.Msg
    | Tick Time
    | LookAngleTable LookAngleTable.Msg
    | LoadMorePasses
    | Fail String



-- Cmds


getTimestamp : Cmd Msg
getTimestamp =
    Time.now |> Task.mapError toString |> Task.perform Fail Timestamp


getLocation : Cmd Msg
getLocation =
    Geolocation.nowWith
        { enableHighAccuracy = False
        , timeout = Just 10000
        , maximumAge = Just (48 * 3600000)
        }
        |> Task.toMaybe
        |> Task.perform Fail Location


getTles : List SatName -> Cmd Msg
getTles sats =
    Http.getString "nasabare.txt"
        |> Task.mapError toString
        |> Task.perform Fail TleString


type alias PassReq =
    { latitude : Float
    , longitude : Float
    , altitude : Float
    , begin : Time
    , end : Time
    , sats : List { satName : SatName, tle : Tle }
    }


port sendPassReq : PassReq -> Cmd msg


nextPassReq : Time -> Model -> PassReq
nextPassReq loadMoreInterval { location, tles, loadedUpTo } =
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
                , end = loadedUpTo + loadMoreInterval
                , sats = sats
                }
           )



-- Subs


port recvPasses : (( Time, List Pass ) -> msg) -> Sub msg


subs : Model -> Sub Msg
subs model =
    Sub.batch
        [ recvPasses Passes
        , Time.every Time.second Tick
        , Sub.map LookAngleTable (LookAngleTable.subs model.lookAngleTable)
        ]



-- Update


update : Context -> Msg -> Model -> ( Model, Cmd Msg )
update context action model =
    case action of
        Timestamp time ->
            ( { model
                | time = time
                , loadedUpTo = time - context.history
              }
            , getLocation
            )

        Location (Just location) ->
            ( { model
                | location = location
                , geoMsg = Absent
                , msg = Present Info "Getting TLEs..."
              }
            , getTles context.sats
            )

        Location Nothing ->
            ( { model
                | geoMsg = Present Warning "Warning: Geolocation failed, fell back to 0°N, 0°E"
                , msg = Present Info "Getting TLEs..."
              }
            , getTles context.sats
            )

        TleString tleStr ->
            let
                tles =
                    parseTle context.sats tleStr

                model' =
                    { model
                        | tles = tles
                        , msg = Present Info "Getting passes..."
                    }
            in
                ( model'
                , sendPassReq (nextPassReq context.loadMoreInterval model')
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
                    , msg = Absent
                  }
                , Cmd.none
                )

        Filter action ->
            ( { model | filter = PassFilter.update action model.filter }
            , Cmd.none
            )

        Tick time ->
            ( { model | time = time }
            , Cmd.none
            )

        LookAngleTable childMsg ->
            let
                ( lookAngleModel, lookAngleCmd ) =
                    LookAngleTable.update model childMsg model.lookAngleTable
            in
                ( { model | lookAngleTable = lookAngleModel }
                , Cmd.map LookAngleTable lookAngleCmd
                )

        LoadMorePasses ->
            ( model
            , sendPassReq (nextPassReq context.loadMoreInterval model)
            )

        Fail msg ->
            ( { model | msg = Present Error msg }
            , Cmd.none
            )



-- View


view : Context -> Model -> Html Msg
view context model =
    let
        filteredPasses =
            model.passes
                |> Dict.filter (\_ p -> PassFilter.pred model.filter p)
                |> Dict.toList
                |> List.map snd
    in
        H.div
            [ HA.class "container"
            , HA.style [ ( "max-width", "980px" ) ]
            ]
            [ infoBox model.geoMsg
            , infoBox model.msg
            , H.h3 [ HA.style [ ( "text-align", "center" ) ] ]
                [ H.text "Current passes" ]
            , LookAngleTable.view model.time model.passes model.lookAngleTable
            , H.div [ HA.style [ ( "height", "5px" ) ] ] []
            , H.h3 [ HA.style [ ( "text-align", "center" ) ] ]
                [ H.text "Future passes" ]
            , App.map Filter (PassFilter.view context.sats model.filter)
            , PassTable.view model.time filteredPasses
            , loadMoreButton
            ]


infoBox : UserMsg -> Html a
infoBox userMsg =
    case userMsg of
        Absent ->
            H.div [] []

        Present level str ->
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
