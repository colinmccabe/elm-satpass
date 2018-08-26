port module Main exposing (main)

import Browser
import Debug
import Dict exposing (Dict)
import Html as H exposing (Html)
import Html.Attributes as HA
import Html.Events
import Html.Lazy
import Http
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
            (\_ ->
                Sub.batch
                    [ recvPasses Passes
                    , Time.every 1000 Tick
                    , recvLookAngles LookAngles
                    ]
            )
        }


type alias Constants =
    { history : Int
    , loadMoreInterval : Int
    , defaultLocation : Location
    , blacklist : List SatName
    }


constants : Constants
constants =
    { history = 30 * 60 * 1000
    , loadMoreInterval = 6 * 60 * 60 * 1000
    , defaultLocation =
        { latitude = 0.0
        , longitude = 0.0
        , altitude = 0
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
    { msg : UserMsg
    , location : Location
    , time : Timestamp
    , tles : Dict SatName Tle
    , passes : Dict PassId Pass
    , lookAngles : Dict PassId LookAngle
    , loadedUpTo : Timestamp
    , filter : PassFilter.Model
    }


init : ( Model, Cmd Msg )
init =
    ( { msg = Show Info "Trying to get location..."
      , location = constants.defaultLocation
      , time = 0
      , tles = Dict.empty
      , passes = Dict.empty
      , lookAngles = Dict.empty
      , loadedUpTo = 0
      , filter = PassFilter.init
      }
    , getTimestamp
    )


type Msg
    = InitTimestamp Timestamp
    | TleString String
    | Passes ( Timestamp, List Pass )
    | LookAngles (List LookAngle)
    | Filter PassFilter.Msg
    | Tick Time.Posix
    | LoadMorePasses
    | Fail String



-- Cmds


getTimestamp : Cmd Msg
getTimestamp =
    Time.now |> Task.map Time.posixToMillis |> Task.perform InitTimestamp


getTles : Cmd Msg
getTles =
    Http.getString "nasabare.txt"
        |> Http.send (\res ->
            case res of
                Ok body -> TleString body
                Err e -> Fail (Debug.toString e)
        )


type alias PassReq =
    { latitude : Float
    , longitude : Float
    , altitude : Float
    , begin : Int
    , end : Int
    , sats : List { satName : SatName, tle : Tle }
    }


port getPasses : PassReq -> Cmd msg


type alias LookAngleReq =
    { time : Int
    , latitude : Float
    , longitude : Float
    , altitude : Float
    , sats : List { id : PassId, tle : Tle }
    }


port getLookAngles : LookAngleReq -> Cmd msg



-- Subs


port recvPasses : (( Int, List Pass ) -> msg) -> Sub msg


port recvLookAngles : (List LookAngle -> msg) -> Sub msg



-- Update


update : Msg -> Model -> ( Model, Cmd Msg )
update action model =
    case action of
        InitTimestamp time ->
            ( { model
                | time = time
                , loadedUpTo = time - constants.history
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

        Filter filterAction ->
            ( { model | filter = PassFilter.update filterAction model.filter }
            , Cmd.none
            )

        Tick posix ->
            let time = Time.posixToMillis posix
            in
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


nextPassReq : Timestamp -> Model -> PassReq
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
                , altitude = location.altitude
                , begin = loadedUpTo
                , end = loadedUpTo + interval
                , sats = sats
                }
           )


nextLookAngleReq : Model -> Timestamp -> LookAngleReq
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
                    , altitude = location.altitude
                    , sats = sats
                    }
               )



-- View


view : Model -> Browser.Document Msg
view model =
    let
        filteredPasses =
            model.passes
                |> Dict.filter (\_ p -> PassFilter.pred model.filter p)
                |> Dict.values
    in
        { title = "SatPass"
        , body =
            [ H.div
                [ HA.class "container"
                , HA.style "max-width" "980px"
                ]
                [ infoBox model.msg
                , H.h3 [ HA.style "text-align" "center" ]
                    [ H.text "Current passes" ]
                , LookAngleTable.view model.time model.passes model.lookAngles
                , H.div [ HA.style "height" "5px" ] []
                , H.h3 [ HA.style "text-align" "center" ]
                    [ H.text "Future passes" ]
                , H.map Filter (PassFilter.view model.filter)
                , PassTable.view model.time filteredPasses
                , loadMoreButton
                ]
            ]
        }



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
                    , HA.style"margin-top" "10px"
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
