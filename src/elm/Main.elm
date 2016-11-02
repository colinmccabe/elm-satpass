port module Main exposing (main)

import Dict exposing (Dict)
import Html as H exposing (Html)
import Html.App as App
import Html.Attributes as HA
import Html.Events
import Html.Lazy
import Http
import Json.Decode as JD exposing ((:=))
import LookAngleTable
import PassFilter
import PassTable
import String
import Task exposing (Task)
import Time exposing (Time)
import Types exposing (..)


main : Program Never
main =
    App.program
        { init = init
        , update = update
        , view = Html.Lazy.lazy view
        , subscriptions = subs
        }


sats : List String
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


type alias Model =
    { msg : UserMsg
    , passes : Dict PassId Pass
    , loadedUpTo : Time
    , time : Time
    , filter : PassFilter.Model
    , lookAngleTable : LookAngleTable.Model
    }


init : ( Model, Cmd Msg )
init =
    ( { msg = Present Info "Getting passes..."
      , passes = Dict.empty
      , loadedUpTo = 0
      , time = 0
      , filter = PassFilter.init
      , lookAngleTable = LookAngleTable.init
      }
    , getTimestamp
    )


type Msg
    = CurrentTime Time
    | Passes Time (List Pass)
    | Filter PassFilter.Msg
    | Tick Time
    | LookAngleTable LookAngleTable.Msg
    | LoadMorePasses
    | Fail String



-- Cmds


getTimestamp : Cmd Msg
getTimestamp =
    Time.now |> Task.perform Fail CurrentTime



-- Subs


subs : Model -> Sub Msg
subs model =
    Sub.batch
        [ Time.every Time.second Tick
        , Sub.map LookAngleTable (LookAngleTable.subs model.lookAngleTable)
        ]



-- Tasks


decodePass : JD.Decoder Pass
decodePass =
    JD.object8 Pass
        (JD.map toString ("startTime" := JD.float))
        ("satName" := JD.string)
        ("maxEl" := JD.int)
        ("startTime" := JD.float)
        ("apogeeTime" := JD.float)
        ("endTime" := JD.float)
        ("startAz" := JD.int)
        ("endAz" := JD.int)


getPasses : Time -> Time -> Cmd Msg
getPasses from to =
    let
        queryParams =
            [ ( "from", toString from )
            , ( "to", toString to )
            , ( "sats", String.join "," sats )
            , ( "min-el", toString 30 )
            ]

        url =
            Http.url "http://colin-tower:8080/satpass/passes" queryParams

        httpTask =
            Http.get (JD.list decodePass) url
                |> Task.mapError toString
    in
        Task.perform Fail (Passes to) httpTask



-- Update


update : Msg -> Model -> ( Model, Cmd Msg )
update action model =
    case action of
        CurrentTime time ->
            let
                from =
                    time - (1 * Time.hour)

                to =
                    time + (4 * Time.hour)
            in
                ( { model
                    | time = time
                    , loadedUpTo = time - (1 * Time.hour)
                  }
                , getPasses from to
                )

        Passes loadedUpTo newPasses ->
            let
                newPassesDict =
                    newPasses
                        |> List.map (\pass -> ( pass.passId, pass ))
                        |> Dict.fromList
            in
                ( { model
                    | passes =
                        Dict.union model.passes newPassesDict
                    , loadedUpTo = loadedUpTo
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
                    LookAngleTable.update model.passes childMsg model.lookAngleTable
            in
                ( { model | lookAngleTable = lookAngleModel }
                , Cmd.map LookAngleTable lookAngleCmd
                )

        LoadMorePasses ->
            let
                from =
                    model.loadedUpTo

                to =
                    model.loadedUpTo + (16 * Time.hour)
            in
                ( model
                , getPasses from to
                )

        Fail msg ->
            ( { model | msg = Present Error msg }
            , Cmd.none
            )



-- View


view : Model -> Html Msg
view model =
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
            [ infoBox model.msg
            , H.h3 [ HA.style [ ( "text-align", "center" ) ] ]
                [ H.text "Current passes" ]
            , LookAngleTable.view model.time model.passes model.lookAngleTable
            , H.div [ HA.style [ ( "height", "5px" ) ] ] []
            , H.h3 [ HA.style [ ( "text-align", "center" ) ] ]
                [ H.text "Future passes" ]
            , App.map Filter (PassFilter.view sats model.filter)
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
                            "bg-danger"
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
