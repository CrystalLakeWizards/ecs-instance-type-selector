module App.Configuration exposing (Cluster, Container, Containers, Controllers, Daemon, Daemons, Model, Msg(..), PackingStrategy(..), Controller, Clusters, PricingFilter(..), getContainers, init, update, view)

-- This is probably the only real "messy" file, could do with some refactoring and clean up

import App.Util as Util
import Bootstrap.Button as Button
import Bootstrap.ListGroup as ListGroup
import Bootstrap.Utilities.Flex as Flex
import Bootstrap.Utilities.Size as Size
import Dict exposing (Dict)
import Dict.Extra exposing (filterMap)
import FeatherIcons
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events.Extra exposing (onChange, onEnter)
import Multiselect
import Tuple exposing (first, second)
import Random

init : Model
init =
    { clusters = Dict.fromList [ ( 0, { name = "Cluster" } ) ]
    , controllers = Dict.fromList [ ]
    , containers = Dict.fromList [ ] 
    , daemons = Dict.fromList [ ]
    , autoIncrement = 0 -- Set this to 0 once we get rid of sample data
    }


type alias Controllers =
    Dict Int Controller


type alias Clusters =
    Dict Int Cluster


type alias Containers =
    Dict Int Container

type alias Daemons = 
    Dict Int Daemon


type alias Model =
    { clusters : Clusters
    , controllers : Controllers
    , containers : Containers
    , daemons: Daemons
    , autoIncrement : Int
    }


type Msg
    = AddCluster
    | AddController Int -- ClusterId
    | AddContainer Int -- ControllerId
    | AddDaemon Int -- ContainerId
    | DeleteContainer Int -- ContainerId
    | DeleteController Int -- ControllerId
    | DeleteCluster Int -- ClusterId
    | DeleteDaemon Int -- DaemonId
    | ChangeContainerName Int String -- ContainerId Name
    | ChangeDaemonName Int String -- DaemonId Name
    | ChangeControllerName Int String -- ControllerId Name
    | ChangeClusterName Int String -- ClusterId Name


type PricingFilter
    = Reserved
    | OnDemand


type alias Cluster =
    { name : String
    }


type alias Controller =
    { name : String
    , clusterId : Int
    , scalingTarget : Int
    , packingStrategy : PackingStrategy
    , minPods : Int
    , maxPods : Int
    , nominalPods: Int
    }


type PackingStrategy
    = ByCPUShares
    | ByMemory


type alias Container =
    { name : String
    , color : String
    , controllerId : Int
    , cpuShare : Int
    , memory : Int
    , ioops : Int
    , isSSD : Bool
    --, useEBS : Bool
    , persistentStorage : Int
    , bandwidth : Int
    , showExtraMemory : Bool
    }


type alias Daemon = 
    { memory: Int
    , cpuShare: Int
    , name: String
    , containerId: Int
    }


generateId : Model -> Int
generateId model =
    model.autoIncrement + 1


update : Msg -> Model -> Model
update msg model =
    case msg of
        AddCluster ->
            { model | clusters = model.clusters |> Dict.insert model.autoIncrement { name = "Cluster"}, autoIncrement = generateId model }

        AddController clusterId ->
            { model | controllers = model.controllers |> Dict.insert model.autoIncrement { name = "Controller", clusterId = clusterId, scalingTarget = 0, packingStrategy = ByCPUShares, minPods = 1, maxPods = 2, nominalPods = 1 }, autoIncrement = generateId model }

        AddContainer controllerId ->
            let
                containerId = generateId model
                daemonId = generateId model
                daemons = model.daemons |> Dict.insert daemonId {name = "Daemon", containerId = containerId, cpuShare = 0, memory = 0}
            in
      --Made a change on line 138 that adjusted for the infromation being put into the dictionary
            { model | containers = model.containers |> Dict.insert containerId { name = "Container", color = Util.randomColorString (Random.initialSeed model.autoIncrement), controllerId = controllerId, cpuShare = 128, memory = 4000, ioops = 128, isSSD = False, persistentStorage = 0, bandwidth = 20, showExtraMemory = False }, daemons = daemons, autoIncrement = containerId + 1 }

        AddDaemon containerId -> 
            { model | daemons = model.daemons |> Dict.insert model.autoIncrement {name = "Daemon", containerId = containerId, cpuShare = 0, memory = 0}, autoIncrement = generateId model }

        DeleteContainer containerId ->
            { model | containers = model.containers |> Dict.remove containerId }

        DeleteDaemon daemonId -> 
            { model | daemons = model.daemons |> Dict.remove daemonId }

        DeleteController controllerId ->
            let
                newModel =
                    { model | containers = model.containers |> Dict.Extra.removeWhen (\_ container -> container.controllerId == controllerId) }
            in
            { newModel | controllers = model.controllers |> Dict.remove controllerId }

        DeleteCluster clusterId ->
            -- This mess could use a refactor
            let
                controllersToRemove =
                    model.controllers |> Dict.filter (\_ controller -> controller.clusterId == clusterId)

                controllerIdsToRemove =
                    controllersToRemove |> Dict.keys

                newContainers =
                    model.containers |> Dict.Extra.removeWhen (\_ container -> List.length (List.filter (\item -> item == container.controllerId) controllerIdsToRemove) > 0)

                newControllers =
                    model.controllers |> Dict.Extra.removeWhen (\_ controller -> controller.clusterId == clusterId)
            in
            { model | containers = newContainers, controllers = newControllers, clusters = model.clusters |> Dict.remove clusterId }

        ChangeContainerName id value ->
            { model | containers = Dict.update id (Maybe.map (\container -> { container | name = value })) model.containers }

        ChangeDaemonName id value ->
            { model | daemons = Dict.update id (Maybe.map (\daemon -> { daemon | name = value })) model.daemons}

        ChangeControllerName id value ->
            { model | controllers = Dict.update id (Maybe.map (\controller -> { controller | name = value })) model.controllers }

        ChangeClusterName id value ->
            { model | clusters = Dict.update id (Maybe.map (\cluster -> { cluster | name = value })) model.clusters }


view : Model -> Html Msg
view model =
    div [ class "px-3", class "pt-1" ]
        [ div []
            [ Util.viewColumnTitle "Configuration"
            , hr [] []
            ]
        , viewClusters model
        , hr [] []
        , ListGroup.custom
            [ simpleListItem "Filters" FeatherIcons.filter [ href "settings" ]
            , simpleListItem "Generate Pod File" FeatherIcons.download [ href "export" ]
            , simpleListItem "Generate Configuration File" FeatherIcons.download [ href "load" ]
            ]
        ]


viewClusters : Model -> Html Msg
viewClusters model =
    div [ style "max-height" "60vh", style "overflow-y" "scroll" ]
        [ ListGroup.custom (List.concatMap (viewClusterItem model) (Dict.toList model.clusters))
        ]


viewClusterItem : Model -> ( Int, Cluster ) -> List (ListGroup.CustomItem Msg)
viewClusterItem model clusterTuple =
    let
        id =
            first clusterTuple

        cluster =
            second clusterTuple
    in
    List.concat
        [ [ ListGroup.anchor
                [ ListGroup.attrs [ Flex.block, Flex.justifyBetween, class "cluster-item", href ("cluster/" ++ String.fromInt id) ] ]
                [ div [ Flex.block, Flex.justifyBetween, Size.w100 ]
                    [ span [ class "pt-1" ] [ FeatherIcons.share2 |> FeatherIcons.withSize 19 |> FeatherIcons.toHtml [], input [ type_ "text", class "editable-label", value cluster.name, onChange (ChangeClusterName id)] [] ]
                    , div []
                        [ span [] [ Button.button [ Button.outlineSecondary, Button.small, Button.attrs [ Html.Events.Extra.onClickPreventDefaultAndStopPropagation (AddController id) ] ] [ FeatherIcons.plus |> FeatherIcons.withSize 16 |> FeatherIcons.withClass "empty-button" |> FeatherIcons.toHtml [], text "" ] ]

                        -- needed to prevent the onClick of the list item from firing, and rerouting us to a non-existant thingy
                        ]
                    ]
                ]
          ]
        , viewControllers model (getControllers id model.controllers)
        ]


getControllers : Int -> Controllers -> Controllers
getControllers clusterId controllers =
    let
        associateController _ controller =
            if controller.clusterId == clusterId then
                Just controller

            else
                Nothing
    in
    controllers |> filterMap associateController


viewControllers : Model -> Controllers -> List (ListGroup.CustomItem Msg)
viewControllers model controllers =
    List.concatMap (viewControllerItem model) (Dict.toList controllers)


viewControllerItem : Model -> ( Int, Controller ) -> List (ListGroup.CustomItem Msg)
viewControllerItem model controllerTuple =
    let
        id =
            first controllerTuple

        controller =
            second controllerTuple
    in
    List.concat
        [ [ ListGroup.anchor
                [ ListGroup.attrs [ Flex.block, Flex.justifyBetween, href ("controller/" ++ String.fromInt id) ] ]
                [ div [ Flex.block, Flex.justifyBetween, Size.w100 ]
                    [ span [ class "pt-1" ] [ FeatherIcons.server |> FeatherIcons.withSize 19 |> FeatherIcons.toHtml [], input [ type_ "text", class "editable-label", value controller.name, onChange (ChangeControllerName id)] []]
                    , span [ class "text-muted", Html.Events.Extra.onClickPreventDefaultAndStopPropagation (DeleteController id) ] [ FeatherIcons.trash2 |> FeatherIcons.withSize 16 |> FeatherIcons.toHtml [] ]
                    ]
                ]
          ]
        , viewPodItem id
        , viewContainers (getContainers id model.containers)
        ]


viewPodItem : Int -> List (ListGroup.CustomItem Msg)
viewPodItem id =
    [ ListGroup.anchor
        [ ListGroup.attrs [ Flex.block, Flex.justifyBetween, style "padding-left" "40px", href ("pod/" ++ String.fromInt id) ] ]
        [ div [ Flex.block, Flex.justifyBetween, Size.w100 ]
            [ span [ class "pt-1" ] [ FeatherIcons.clipboard |> FeatherIcons.withSize 19 |> FeatherIcons.toHtml [], text "Pods" ]
            , span [] [ Button.button [ Button.outlineSecondary, Button.small, Button.attrs [ Html.Events.Extra.onClickPreventDefaultAndStopPropagation (AddContainer id) ] ] [ FeatherIcons.plus |> FeatherIcons.withSize 16 |> FeatherIcons.withClass "empty-button" |> FeatherIcons.toHtml [], text "" ] ]
            ]
        ]
    ]


getContainers : Int -> Containers -> Containers
getContainers controllerId containers =
    let
        associateContainer _ container =
            if container.controllerId == controllerId then
                Just container

            else
                Nothing
    in
    containers |> filterMap associateContainer


viewContainers : Containers -> List (ListGroup.CustomItem Msg)
viewContainers containers =
    List.map viewContainerItem (Dict.toList containers)


viewContainerItem : ( Int, Container ) -> ListGroup.CustomItem Msg
viewContainerItem containerTuple =
    let
        id =
            first containerTuple

        container =
            second containerTuple
    in
    ListGroup.anchor [ ListGroup.attrs [ href ("container/" ++ String.fromInt id), style "padding-left" "60px", style "border-left" ("4px solid " ++ container.color)] ]
        [ FeatherIcons.box |> FeatherIcons.withSize 19 |> FeatherIcons.toHtml []
        , input [ type_ "text", class "editable-label", value container.name, onChange (ChangeContainerName id)] []
        , span [ class "ml-3 text-muted float-right", Html.Events.Extra.onClickPreventDefaultAndStopPropagation (DeleteContainer id) ] [ FeatherIcons.trash2 |> FeatherIcons.withSize 16 |> FeatherIcons.toHtml [] ]
        ]


simpleListItem : String -> FeatherIcons.Icon -> List (Html.Attribute Msg) -> ListGroup.CustomItem Msg
simpleListItem label icon attrs =
    ListGroup.anchor [ ListGroup.attrs attrs ] [ icon |> FeatherIcons.withSize 19 |> FeatherIcons.toHtml [], text label ]
