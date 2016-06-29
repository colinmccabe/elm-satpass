port module Main exposing (main)

import Dict exposing (Dict)
import PassFilter
import Html as H exposing (Html)
import Html.App as App
import Html.Attributes as HA
import Html.Events
import Http
import LookAngleTable
import PassTable
import Task exposing (Task)
import Time exposing (Time)
import Types exposing (..)


duration : Time
duration =
    16 * Time.hour


sats : List SatName
sats =
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


main : Program Never
main =
    App.program
        { init = init
        , update = update
        , view = view
        , subscriptions = subs
        }


type alias Model =
    { status : String
    , coords : Coords
    , time : Time
    , tles : Dict SatName Tle
    , passes : Dict PassId Pass
    , loadedUpTo : Time
    , filter : PassFilter.Model
    , lookAngleTable : LookAngleTable.Model
    }


init : ( Model, Cmd Msg )
init =
    ( { status = "Trying to get location..."
      , coords = { latitude = 0.0, longitude = 0.0, altitude = Nothing }
      , time = 0.0
      , tles = Dict.empty
      , passes = Dict.empty
      , loadedUpTo = 0.0
      , filter = PassFilter.init
      , lookAngleTable = LookAngleTable.init
      }
    , Cmd.none
    )


type Msg
    = TimeAndCoords ( Time, Coords )
    | Tle (Dict SatName Tle)
    | Passes ( Time, List Pass )
    | Filter PassFilter.Msg
    | Tick Time
    | LookAngleTable LookAngleTable.Msg
    | LoadMorePasses
    | Fail String



-- Cmds


getTles : List SatName -> Cmd Msg
getTles sats =
    Http.getString "nasabare.txt"
        |> (flip Task.onError) (\_ -> Http.getString "https://s3.amazonaws.com/cmccabe/keps/nasabare.txt")
        |> Task.map (parseTle sats)
        |> Task.mapError toString
        |> Task.perform Fail Tle


type alias PassReq =
    { coords : Coords
    , begin : Time
    , end : Time
    , sats : List { satName : SatName, tle : Tle }
    }


port sendPassReq : PassReq -> Cmd msg


nextPassReq : Model -> PassReq
nextPassReq { coords, tles, loadedUpTo } =
    tles
        |> Dict.toList
        |> List.map
            (\( satName, tle ) ->
                { satName = satName
                , tle = tle
                }
            )
        |> (\sats ->
                { coords = coords
                , begin = loadedUpTo
                , end = loadedUpTo + duration
                , sats = sats
                }
           )



-- Subs


port recvTimeAndCoords : (( Time, Coords ) -> msg) -> Sub msg


port recvPasses : (( Time, List Pass ) -> msg) -> Sub msg


subs : Model -> Sub Msg
subs model =
    Sub.batch
        [ recvTimeAndCoords TimeAndCoords
        , recvPasses Passes
        , Time.every Time.second Tick
        , Sub.map LookAngleTable (LookAngleTable.subs model.lookAngleTable)
        ]



-- Update


update : Msg -> Model -> ( Model, Cmd Msg )
update action model =
    case action of
        TimeAndCoords ( time, coords ) ->
            ( { model
                | coords = coords
                , time = time
                , loadedUpTo = time - 1 * Time.hour
                , status = "Trying to get TLEs..."
              }
            , getTles sats
            )

        Tle tles ->
            let
                model' =
                    { model | tles = tles, status = "Trying to get passes..." }
            in
                ( model'
                , sendPassReq (nextPassReq model')
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
                    , status = "No passes meet criteria"
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
            , sendPassReq (nextPassReq model)
            )

        Fail msg ->
            ( { model | status = msg }
            , Cmd.none
            )



-- View


view : Model -> Html Msg
view model =
    let
        filteredPasses =
            model.passes
                |> Dict.toList
                |> List.map snd
                |> List.filter (PassFilter.pred model.filter)

        passTable =
            case filteredPasses of
                [] ->
                    H.div [] [ H.text model.status ]

                _ ->
                    PassTable.view model.time filteredPasses
    in
        H.div [ HA.class "container" ]
            [ LookAngleTable.view model.time model.passes model.lookAngleTable
            , H.div [ HA.style [ ( "height", "5px" ) ] ] []
            , H.h3 [ HA.style [ ( "text-align", "center" ) ] ]
                [ H.text "Future passes" ]
            , App.map Filter (PassFilter.view sats model.filter)
            , passTable
            , loadMoreButton
            ]


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
