module Main exposing (main)

import Dict exposing (Dict)
import Html as H exposing (Html)
import Html
import Html.Attributes as HA
import Html.Events
import Html.Lazy
import Http
import Json.Decode as JD
import LookAngleTable
import PassFilter
import PassTable
import Result.Extra
import Task exposing (Task)
import Time exposing (Time)
import Types exposing (..)


serverHost : String
serverHost =
    "beaglebone"


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


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , update = update
        , view = Html.Lazy.lazy view
        , subscriptions = subs
        }


type alias Model =
    { time : Time
    , lookAngles : List ( Pass, LookAngle )
    , passes : Result String (Dict PassId Pass)
    , loadedUpTo : Time
    , filter : PassFilter.Model
    }


init : ( Model, Cmd Msg )
init =
    ( { passes = Err "Loading..."
      , lookAngles = []
      , loadedUpTo = 0
      , time = 0
      , filter = PassFilter.init
      }
    , getTimestamp
    )


type Msg
    = InitalTick Time
    | Passes (Result String ( Time, List Pass ))
    | Filter PassFilter.Msg
    | Tick Time
    | LookAngles (Result String (Dict String LookAngle))
    | LoadMorePasses



-- Cmds


getTimestamp : Cmd Msg
getTimestamp =
    Time.now |> Task.perform InitalTick



-- Subs


subs : Model -> Sub Msg
subs model =
    Time.every Time.second Tick



-- Tasks


getPasses : Time -> Time -> Cmd Msg
getPasses from to =
    let
        path =
            "http://" ++ serverHost ++ ":8080/satpass/passes"

        queryParams =
            [ ( "from", toString from )
            , ( "to", toString to )
            , ( "min-el", toString 5 )
            , ( "sats", String.join "," sats )
            ]

        url =
            buildUrl path queryParams

        decodePass =
            JD.map8 Pass
                (JD.field "passId" JD.string)
                (JD.field "satName" JD.string)
                (JD.field "maxEl" JD.int)
                (JD.field "startTime" JD.float)
                (JD.field "apogeeTime" JD.float)
                (JD.field "endTime" JD.float)
                (JD.field "startAz" JD.int)
                (JD.field "endAz" JD.int)
    in
        Http.get url (JD.list decodePass)
            |> Http.send
                (Result.Extra.unpack
                    (\err -> Passes (Err (toString err)))
                    (\passes -> Passes (Ok ( to, passes )))
                )


satList : Dict PassId Pass -> Time -> Dict SatName PassId
satList passes time =
    passes
        |> Dict.filter (\_ pass -> time > pass.startTime && time < pass.endTime)
        |> Dict.toList
        |> List.map (\( _, pass ) -> ( pass.satName, pass.passId ))
        |> Dict.fromList


getLookAngles : Dict SatName PassId -> Cmd Msg
getLookAngles nameToIdDict =
    if Dict.isEmpty nameToIdDict then
        Cmd.none
    else
        let
            satNames =
                Dict.keys nameToIdDict

            path =
                "http://" ++ serverHost ++ ":8080/satpass/pos"

            queryString =
                [ ( "sats", String.join "," (List.map Http.encodeUri satNames) )
                ]

            url =
                buildUrl path queryString

            decodePos =
                JD.map2 LookAngle
                    (JD.field "elevation" JD.float)
                    (JD.field "azimuth" JD.float)

            -- Server returns a map of SatNames to LookAngles
            -- Change this to a map of PassIds to LookAngles
            swapKeys nameToAngleDict =
                Dict.merge (\_ _ _ -> Debug.crash "extra lookAngle entry")
                    (\_ lookAngle passId acc -> Dict.insert passId lookAngle acc)
                    (\_ _ _ -> Debug.crash "extra passId entry")
                    nameToAngleDict
                    nameToIdDict
                    Dict.empty
        in
            Http.get url (JD.dict decodePos)
                |> Http.send
                    (Result.Extra.unpack
                        (\err -> LookAngles (Err (toString err)))
                        (\angles -> LookAngles (Ok (swapKeys angles)))
                    )



-- Update


update : Msg -> Model -> ( Model, Cmd Msg )
update action model =
    case action of
        InitalTick time ->
            let
                from =
                    time - (1 * Time.hour)

                to =
                    time + (16 * Time.hour)
            in
                ( { model
                    | time = time
                    , loadedUpTo = time - (1 * Time.hour)
                  }
                , getPasses from to
                )

        Passes (Ok ( loadedUpTo, newPasses )) ->
            let
                passes =
                    newPasses
                        |> List.map (\pass -> ( pass.passId, pass ))
                        |> Dict.fromList
                        |> Dict.union (Result.withDefault Dict.empty model.passes)
            in
                ( { model
                    | passes = Ok passes
                    , loadedUpTo = loadedUpTo
                  }
                , getLookAngles (satList passes model.time)
                )

        Passes (Err msg) ->
            ( { model | passes = Err msg }
            , Cmd.none
            )

        Filter action ->
            ( { model | filter = PassFilter.update action model.filter }
            , Cmd.none
            )

        Tick time ->
            ( { model | time = time }
            , case model.passes of
                Ok passes ->
                    getLookAngles (satList passes model.time)

                Err _ ->
                    Cmd.none
            )

        LookAngles (Ok lookAnglesDict) ->
            let
                newLookAngles passes =
                    Dict.merge (\_ _ _ -> Debug.crash "LookAngle with no pass")
                        (\_ angle pass acc -> ( pass, angle ) :: acc)
                        (\_ _ acc -> acc)
                        lookAnglesDict
                        passes
                        []

                lookAngles =
                    Result.Extra.unpack (\_ -> [])
                        newLookAngles
                        model.passes
            in
                ( { model | lookAngles = lookAngles }
                , Cmd.none
                )

        LookAngles (Err msg) ->
            ( { model | lookAngles = [] }
            , Cmd.none
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



-- View


view : Model -> Html Msg
view model =
    let
        filterPasses passes =
            passes
                |> Dict.filter (\_ p -> PassFilter.pred model.filter p)
                |> Dict.toList
                |> List.map Tuple.second

        filteredPasses =
            Result.map filterPasses model.passes
    in
        H.div
            [ HA.class "container"
            , HA.style [ ( "max-width", "980px" ) ]
            ]
            [ H.h3 [ HA.style [ ( "text-align", "center" ) ] ]
                [ H.text "Current passes" ]
            , LookAngleTable.view model.time model.lookAngles
            , H.div [ HA.style [ ( "height", "5px" ) ] ] []
            , H.h3 [ HA.style [ ( "text-align", "center" ) ] ]
                [ H.text "All passes" ]
            , Html.map Filter (PassFilter.view sats model.filter)
            , PassTable.view model.time filteredPasses
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
