module App.Daemon exposing (Model, Msg(..), update, view, sumDaemonResources, daemonsForNode)

import App.Configuration as Configuration
import App.Util as Util
import Bootstrap.Button as Button
import Bootstrap.Card as Card
import Bootstrap.Card.Block as Block
import Bootstrap.Form as Form
import Dict exposing (Dict)
import FeatherIcons
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import Html.Attributes exposing (..)
import Html.Events.Extra exposing (onChange, onEnter)
import Bootstrap.Breadcrumb exposing (container)
import Bootstrap.Utilities.DomHelper exposing (className)

-- https://learnyouanelm.github.io/pages/07-modules.html

type alias Model = 
    Configuration.Model

type Msg
    = UpdateCPUShare Int String
    | UpdateMemory Int String
    | ConfigurationMsg Configuration.Msg

update : Msg -> Model -> Model
update msg model =
    case msg of
        ConfigurationMsg configMsg ->
            Configuration.update configMsg model
        UpdateCPUShare daemonid val ->
            { model | daemons = Dict.update daemonid (Maybe.map (\daemon -> {daemon | cpuShare = Util.toInt val})) model.daemons}
        UpdateMemory daemonid val ->
            { model | daemons = Dict.update daemonid (Maybe.map (\daemon -> {daemon | memory = Util.toInt val})) model.daemons}

daemonsForNode : Configuration.Daemons -> Int -> List (Int, Configuration.Daemon)
daemonsForNode daemons nodeid =
    let
        filtered = Dict.filter (\_ daemon -> daemon.nodeId == nodeid) daemons
    in
        Dict.toList filtered

-- sumDeamonResources: allDaemons, nodeId -> (cpuShares, memory)
sumDaemonResources : Configuration.Daemons -> Int -> (Int, Int)
sumDaemonResources allDaemons nodeId =
    let
        daemons = daemonsForNode allDaemons nodeId
        cpuShares = List.sum (List.map (\daemonTuple -> (Tuple.second daemonTuple).cpuShare) daemons)
        memory = List.sum (List.map (\daemonTuple -> (Tuple.second daemonTuple).memory) daemons)
    in
        (cpuShares, memory)

viewDaemon : Configuration.Node -> (Int, Configuration.Daemon) -> Html Msg
viewDaemon node (daemonid, daemon) = 
        Card.config [ Card.attrs [Html.Attributes.class "mt-3"]]
        |> Card.header [] [ Html.map ConfigurationMsg (input [ type_ "text", class "editable-label", value daemon.name, onChange (Configuration.ChangeDaemonName daemonid)] [])
        , Html.map ConfigurationMsg (span [ class "ml-3 text-muted float-right clickable", Html.Events.Extra.onClickPreventDefaultAndStopPropagation (Configuration.DeleteDaemon daemonid) ] [ FeatherIcons.trash2 |> FeatherIcons.withSize 16 |> FeatherIcons.toHtml [] ]) ]
        |> Card.block []
            [ Block.custom <|
                Form.form []
                    -- these Util calls are a bit odd, but do make the code a bit more organized.
                    [ Util.viewFormRowSlider "CPU Share" ((String.fromInt <| daemon.cpuShare) ++ "/" ++ (node.cpuShare |> String.fromInt) ++ " CPU Share") daemon.cpuShare 0 node.cpuShare 2 (UpdateCPUShare daemonid)
                    , hr [] []
                    , Util.viewFormRowSlider "Memory" (Util.formatMegabytes daemon.memory ++ "/"  ++ Util.formatMegabytes node.memory) daemon.memory 50 node.memory 50 (UpdateMemory daemonid)
                    ]
            ]
        |> Card.view


view : Configuration.Daemons -> Int -> Configuration.Node -> Html Msg
view daemons nodeId node = 
    let
        kvPairs = daemonsForNode daemons nodeId
        data = List.map (viewDaemon node) kvPairs
    in
        div [] [ Html.map ConfigurationMsg (
                    Button.button [ 
                            Button.outlineSecondary, Button.small, Button.attrs [ 
                                    Html.Events.Extra.onClickPreventDefaultAndStopPropagation (
                                            Configuration.AddDaemon nodeId
                                        ) 
                                    ] 
                                ]
                                 [ 
                                     FeatherIcons.plus |> FeatherIcons.withSize 16 |> FeatherIcons.withClass "empty-button" |> FeatherIcons.toHtml [], text ""
                                ]
                    )
            , div [] data
        ]
        