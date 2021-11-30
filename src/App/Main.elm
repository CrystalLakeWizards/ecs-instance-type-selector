module App.Main exposing (..)

import App.Cluster as Cluster
import App.Configuration as Configuration
import App.Nodes as Nodes
import App.Container as Container
import App.Results as Results
import App.Controller as Controller
import App.Settings as Settings exposing (update, Msg)
import App.Pod as Pod
import App.Util as Util
import Bootstrap.Alert as Alert
import Bootstrap.Button as Button
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Grid.Row as Row
import Bootstrap.Navbar as Navbar
import Bootstrap.Utilities.Spacing as Spacing
import Browser exposing (UrlRequest(..), application, document)
import Browser.Events as BrowserEvent
import Browser.Navigation as Nav
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Json.Decode exposing (Error(..), decodeString)
import Tuple exposing (first, second)
import Url exposing (..)
import Url.Parser as Url exposing ((</>), Parser)
import Multiselect
import FeatherIcons



---- MODEL ----


type alias Flags =
    { basePath : String }


type alias Navigation =
    { key : Nav.Key
    , navbarState : Navbar.State
    , currentDetail : Detail
    }


type alias Model =
    { flags : Flags
    , navigation : Navigation
    , configuration : Configuration.Model
    , nodes : Nodes.Model
    , error : Maybe String
    , settings : Settings.Model
    , collapsedSidebar : Bool
    , viewportSize: (Int, Int)
    }


-- These Nodes model should probably moved in to their own files
-- at some point

type Detail
    = None
    | Cluster Int
    | Controller Int
    | Pod Int
    | Container Int
    | Settings
    | GeneratePodFile
    | GenerateConfiguration



---- UPDATE ----


type Msg
    = NavbarMsg Navbar.State
    | LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | ConfigurationMsg Configuration.Msg
    | NodesMsg Nodes.Msg
    | ControllerMsg Controller.Msg
    | PodMsg Pod.Msg
    | ContainerMsg Container.Msg
    | SettingsMsg Settings.Msg
    | ViewportResize Int Int
    | ToggleSidebar


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ flags, navigation } as model) =
    case msg of
        NavbarMsg state ->
            let
                _ = Debug.log "Nav" state
            in
            ( { model
                | navigation = { navigation | navbarState = state }
              }
            , Cmd.none
            )

        LinkClicked urlRequest ->
            case urlRequest of
                Internal url ->
                    ( model
                    , Nav.pushUrl model.navigation.key (Url.toString url)
                    )

                External url ->
                    ( model
                    , Nav.load url
                    )

        UrlChanged url ->
            ( { model | navigation = { navigation | currentDetail = urlToDetail flags.basePath url } }
            , Cmd.none
            )

        NodesMsg nodesMsg ->
            let
                msgWithCmd =
                    Nodes.update nodesMsg model.nodes
            in
            ( { model | nodes = first msgWithCmd }, Cmd.map NodesMsg (second msgWithCmd))

        ConfigurationMsg configurationMsg ->
            ( { model | configuration = Configuration.update configurationMsg model.configuration }, Cmd.none )

        ControllerMsg controllerMsg ->
            ( { model | configuration = Controller.update controllerMsg model.configuration }, Cmd.none )

        PodMsg podMsg ->
            ( { model | configuration = Pod.update podMsg model.configuration }, Cmd.none )

        ContainerMsg containerMsg ->
            ( { model | configuration = Container.update containerMsg model.configuration }, Cmd.none )

        SettingsMsg settingsMsg ->
            let
                msgWithCmd =
                    Settings.update settingsMsg model.settings

                settingsState = first msgWithCmd     

                oses = (List.map (\item -> Tuple.first item) (Multiselect.getSelectedValues settingsState.excludedSystems))
                nodesExclude = (List.map (\item -> Tuple.first item) (Multiselect.getSelectedValues settingsState.excludedNodes))
                regionsInclude = (List.map (\item -> Tuple.first item) (Multiselect.getSelectedValues settingsState.includedRegions))
                nodes2 = Nodes.update (Nodes.SetFilters (Nodes.OS) oses) model.nodes
                nodes3 = Nodes.update (Nodes.SetFilters (Nodes.NodeType) nodesExclude) (Tuple.first nodes2)
                nodes4 = Nodes.update (Nodes.SetFilters (Nodes.Region) regionsInclude) (Tuple.first nodes3)
                nodes5 = Nodes.update (Nodes.SetPreferredPricing settingsState.preferredPricing) (Tuple.first nodes4)
                nodes = Tuple.first nodes5
            in
            ( { model | settings = first msgWithCmd, nodes = nodes }, Cmd.map SettingsMsg (second msgWithCmd) )

        ToggleSidebar ->
            ( { model | collapsedSidebar = not model.collapsedSidebar }, Cmd.none )     
            
        ViewportResize width height ->
            ( { model | viewportSize = (width, height)}, Cmd.none )


urlToDetail : String -> Url -> Detail
urlToDetail basePath url =
    let
        newUrl =
            case basePath of
                "/" ->
                    url

                _ ->
                    { url | path = String.replace basePath "" url.path }
    in
    newUrl
        |> Url.parse urlParser
        |> Maybe.withDefault None


urlParser : Parser (Detail -> a) a
urlParser =
    Url.oneOf
        [ Url.map None Url.top
        , Url.map Cluster (Url.s "cluster" </> Url.int)
        , Url.map Controller (Url.s "controller" </> Url.int)
        , Url.map Container (Url.s "container" </> Url.int)
        , Url.map Pod (Url.s "pod" </> Url.int)
        , Url.map Settings (Url.s "settings")
        , Url.map GenerateConfiguration (Url.s "load")
        , Url.map GeneratePodFile (Url.s "export")
        ]



---- VIEW ----


view : Model -> Browser.Document Msg
view model =
    { title = "Cluster Prophet"
    , body =
        [ viewNavbar model
        , viewContent model
        , viewToggleButton model.collapsedSidebar
        ]
    }


viewToggleButton : Bool -> Html Msg
viewToggleButton collapsedSidebar =
    let
        (icon, offset) = 
            case collapsedSidebar of
                True -> (FeatherIcons.arrowRight, "0")
                False -> (FeatherIcons.arrowLeft, "25%")
    in
    
    div [ class "toggle-sidebar-container"
        , style "left" offset ] 
        [ Button.button 
            [ Button.secondary, Button.onClick ToggleSidebar ] 
            [ icon |> FeatherIcons.withSize 18 |> FeatherIcons.toHtml [] ] 
        ]


viewContent : Model -> Html Msg
viewContent model =
    let
        columns = [ viewSidebarColumn model, 
                    viewDetailColumn model,
                    viewResultsColumn model
                  ]

        renderedColumns = 
            case model.collapsedSidebar of
                True -> Maybe.withDefault [] (List.tail columns) 
                False -> columns
    in
    Grid.containerFluid [ class "full-height" ]
        [ Grid.row [ Row.attrs [ class "h-100 pt-5" ] ]
            renderedColumns
        ]


viewSidebarColumn : Model -> Grid.Column Msg
viewSidebarColumn model =
    Grid.col [ Col.md3, Col.attrs [ class "p-0 bg-light sidebar"] ]
     [ Html.map ConfigurationMsg (Configuration.view model.configuration) ]


viewDetailColumn : Model -> Grid.Column Msg
viewDetailColumn model =
    Grid.col [ Col.md4, Col.attrs [ class "p-0 bg-light sidebar" ] ]
        [ div [ class "px-3", class "pt-1" ]
            [ Util.viewColumnTitle "Detail"
            , hr [] []
            , viewDetail model
            ]
        ]


viewResultsColumn : Model -> Grid.Column Msg
viewResultsColumn model =
    let
        gridSize = 
            if model.collapsedSidebar then
                Col.md8
            else
                Col.md5
    in
    Grid.col [ gridSize, Col.attrs [ class "p-0" ] ]
             [ Maybe.map viewError model.error |> Maybe.withDefault (span [] [])
                , Results.view (Results.Model model.configuration model.nodes model.settings model.viewportSize model.collapsedSidebar)
             ]


viewDetail : Model -> Html Msg
viewDetail model =
    case model.navigation.currentDetail of
        Cluster id ->
            Dict.get id model.configuration.clusters
                |> Maybe.map (\value -> Cluster.view id value)
                |> Maybe.withDefault viewNotFoundDetail

        Controller id ->
            Dict.get id model.configuration.controllers
                |> Maybe.map (\value -> Html.map ControllerMsg (Controller.view id value))
                |> Maybe.withDefault viewNotFoundDetail

        Pod id ->
            Dict.get id model.configuration.controllers
                |> Maybe.map (\value -> Html.map PodMsg (Pod.view id value (Configuration.getContainers id model.configuration.containers)))
                |> Maybe.withDefault viewNotFoundDetail

        Container id ->
            Dict.get id model.configuration.containers
                |> Maybe.map (\value -> Html.map ContainerMsg (Container.view id value model.configuration.daemons))
                |> Maybe.withDefault viewNotFoundDetail

        Settings ->
            Html.map SettingsMsg (Settings.view model.settings)

        GenerateConfiguration ->
            viewImportDetail model

        GeneratePodFile ->
            viewExportDetail model

        _ ->
            viewNoneDetail


viewImportDetail : Model -> Html Msg
viewImportDetail model =
    let
        servicesList = Dict.toList model.configuration.services
        containersList = Dict.toList model.configuration.containers
        daemonsList = Dict.toList model.configuration.daemons
    in
        pre [ class "text-muted align-middle" ]
            [
                text "Copy the below configuration to a text file so that you can \nreturn to your work at a later time.\n\n"
                , text "\nServices:\n"
                , span [] (List.map printServices servicesList)
                , text "\nContainers:\n"
                , span [] (List.map printContainers containersList)
                , text "\nDaemons:\n"
                , span [] (List.map printDaemons daemonsList)
                , text "\n\n"
                --, text "\nFilters:\n"
                -- TODO, span [] (print out filters no loop needed)
            ]


printServices : (Int, Service) -> Html Msg
printServices service =
    pre [class "text-muted align-middle"]
        [
            text ("- name: " ++ (second service).name ++ "\n")
            , text ("  scaling-target: " ++ (String.fromInt (second service).scalingTarget) ++ "\n")
            , text ("  packing-strategy: " ++  (printVariant (second service)).varType ++ "\n")
            , text ("  min-tasks: " ++ (String.fromInt (second service).minTasks) ++ "\n")
            , text ("  max-tasks: " ++ (String.fromInt (second service).maxTasks) ++ "\n")
            , text ("  nominal-tasks: " ++ (String.fromInt (second service).nominalTasks) ++ "\n")
        ]


printVariant : Service -> {varType:String}
printVariant service =
    let
        x = {varType = ""}
    in
        case service.packingStrategy of
            Configuration.ByCPUShares ->
                {x | varType = "By CPU Shares"}

            Configuration.ByMemory ->
                {x | varType = "By Memory"}


printContainers : (Int, Container) -> Html Msg
printContainers container =
    pre [class "text-muted align-middle"]
    [
        text ("- name: " ++ (second container).name ++ "\n")
        , text ("  cpuShare: " ++ (String.fromInt (second container).cpuShare) ++ "m\n")
        , text ("  memory: " ++ (String.fromInt (second container).memory) ++ "Mi\n")
        , text ("  IOOPs: " ++ (String.fromInt (second container).ioops) ++ "\n")
        , text ("    use-elastic-block-storage: " ++ (printBool (second container).useEBS) ++ "\n")
        , text ("  bandwidth: " ++ (String.fromInt (second container).bandwidth) ++ "Gi\n")
        , text ("  service-id: " ++ (String.fromInt (second container).serviceId) ++ "\n")
        -- TODO add service name to avoid confusion
    ]


printBool : Bool -> String
printBool boolean =
    if boolean then
        "True"

    else
        "False"


printDaemons : (Int, Daemon) -> Html Msg
printDaemons daemon =
    pre [class "text-muted align-middle"]
    [
        text ("- name: " ++ (second daemon).name ++ "\n")
        , text ("  memory: " ++ (String.fromInt (second daemon).memory) ++ "Mi\n")
        , text ("  cpuShare: " ++ (String.fromInt (second daemon).cpuShare) ++ "m\n")
        , text ("  container-id: " ++ (String.fromInt (second daemon).containerId) ++ "\n")
    ]


viewExportDetail : Model -> Html Msg
viewExportDetail model =
    let
        contsList = Dict.toList model.configuration.containers
    in

        pre [ class "text-muted align-middle" ]
            [
                text "This template is to help you create Kubernetes Pod and Deployment files.\nAll container information is presented in yaml format below.\n\n---\napiVersion: v1\nkind: Pod\nmetadata:\n  name: <ENTER NAME>\nspec:\n  containers:"
                , span [] (List.map yamlPrintCont contsList)
                , text "---"
            ]


yamlPrintCont : (Int, Container) -> Html Msg
yamlPrintCont container =
    pre [class "text-muted align-middle"]
    [
        text ("  - name: " ++ (second container).name ++ "\n")
        , text ("    image: <IMAGE NAME>\n")
        , text ("    resources:\n")
        , text ("      requests:\n")
        , text ("        memory: " ++ (String.fromInt (second container).memory) ++ "Mi\n")
        , text ("        cpuShare: " ++ (String.fromInt (second container).cpuShare) ++ "m")
    ]


viewNoneDetail : Html Msg
viewNoneDetail =
    span [ class "text-muted align-middle" ]
        [ text "Nothing here. Select a controller, pod, or container from the left sidebar to start configuring." ]


viewNotFoundDetail : Html Msg
viewNotFoundDetail =
    span [ class "text-muted align-middle" ]
        [ text "Whatever you are looking for does not exist." ]


viewError : String -> Html Msg
viewError error =
    div []
        [ Alert.simpleDanger [] [ text error ] ]


viewNavbar : Model -> Html Msg
viewNavbar model =
    Navbar.config NavbarMsg
        |> Navbar.attrs [ class "flex-md-nowrap", class "p-0", class "shadow" ]
        |> Navbar.fixTop
        |> Navbar.withAnimation
        |> Navbar.dark
        |> Navbar.brand [ href "/", class "text-center", class "col-sm-3", class "col-md-3", class "mr-0", class "p-2" ]
            [ img [ src (model.flags.basePath ++ "ec2.svg"), class "logo" ] [], text "Cluster Prophet" ]
        |> Navbar.customItems
            [ Navbar.textItem [ Spacing.p2Sm, class "muted" ] [ text ("Loaded " ++ (String.fromInt <| List.length model.nodes.nodes) ++ " total nodes") ]
            ]
        |> Navbar.view model.navigation.navbarState



---- HELPERS ----
---- SUBSCRIPTION ----


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Navbar.subscriptions model.navigation.navbarState NavbarMsg
        , Sub.map SettingsMsg <| Settings.subscriptions model.settings
        , Sub.map NodesMsg <| Nodes.subscriptions model.nodes
        , BrowserEvent.onResize (\w h -> ViewportResize w h)
        ]



---- PROGRAM ----




init : Flags -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init ({ basePath } as flags) url key =
    let
        ( navbarState, navbarCmd ) =
            Navbar.initialState NavbarMsg
    in
    ( { flags = flags
      , navigation =
            { key = key
            , navbarState = navbarState
            , currentDetail = urlToDetail basePath url
            }
      , configuration = Configuration.init
      , nodes = Nodes.init
      , error = Nothing
      , settings = Settings.init
      , collapsedSidebar = False
      , viewportSize = (0, 0)
      }
    , Cmd.batch [ navbarCmd, Nodes.requestNodes ( Nodes.defaultRegion, "", Nodes.numNodesBatched ) ]
    )


main : Program Flags Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlChange = UrlChanged
        , onUrlRequest = LinkClicked
        }
