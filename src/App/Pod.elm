module App.Pod exposing (Model, Msg(..), update, view)

import App.Configuration as Configuration
import App.Constants exposing (allRegions)
import App.Util as Util exposing (viewFormRowSlider)
import Bootstrap.Card as Card
import Bootstrap.Card.Block as Block
import Bootstrap.Form as Form
import Bootstrap.Grid.Col as Col
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import Multiselect
import Tuple exposing (first, second)


type alias Model =
    Configuration.Model


type Msg
    = UpdateMinPods Int String
    | UpdateMaxPods Int String
    | UpdateNominalPods Int String



-- https://elmseeds.thaterikperson.com/elm-multiselect


update : Msg -> Model -> Model
update msg model =
    case msg of
        UpdateMinPods id value ->
            { model | controllers = Dict.update id (Maybe.map (\controller -> { controller | minPods = Util.toInt value })) model.controllers }

        UpdateMaxPods id value ->
            { model | controllers = Dict.update id (Maybe.map (\controller -> { controller | maxPods = Util.toInt value })) model.controllers }

        UpdateNominalPods id value ->
            { model | controllers = Dict.update id (Maybe.map (\controller -> { controller | nominalPods = Util.toInt value })) model.controllers }


view : Int -> Configuration.Controller -> Configuration.Containers -> Html Msg
view id controller containers =
    div []
        [ Card.config [ Card.attrs [ class "mt-3" ] ]
            |> Card.header [] [ text (controller.name ++ " - Pod Settings") ]
            |> Card.block []
                [ Block.custom <|
                    Form.form []
                        [ Util.viewFormRowSlider "Min. Pods" ((String.fromInt <| controller.minPods) ++ " Pods") controller.minPods 1 controller.maxPods 1 (UpdateMinPods id)
                        , Util.viewFormRowSlider "Nom. Pods" ((String.fromInt <| controller.nominalPods) ++ " Pods") controller.nominalPods controller.minPods controller.maxPods 1 (UpdateNominalPods id)
                        , Util.viewFormRowSlider "Max. Pods" ((String.fromInt <| controller.maxPods) ++ " Pods") controller.maxPods controller.minPods 100 1 (UpdateMaxPods id)
                        ]
                ]
            |> Card.view
        , Card.config [ Card.attrs [ class "mt-3" ] ]
            |> Card.header [] [ text "Containers Overview" ]
            |> Card.block []
                [ Block.custom <|
                    Form.form []
                        [ Util.viewFormLabel "Total Memory" "Total memory of all containers in this controller combined." ((String.fromFloat <| sumMemory containers * toFloat controller.nominalPods) ++ " GiB")
                        , Util.viewFormLabel "Total CPU Shares" "CPU Shares required for all containers in one pod" ((String.fromInt <| sumCPUShare containers * controller.nominalPods) ++ "/1024")
                        , Util.viewFormLabel "Total Bandwidth" "Bandwidth required for all containers in one pod" ((String.fromInt <| sumBandwidth containers * controller.nominalPods) ++ " GiB/sec")
                        , Util.viewFormLabel "IO Total" "IO requirements for all containers in one pod" (sumIoops controller containers)
                        ]
                ]
            |> Card.view
        ]


sumMemory : Configuration.Containers -> Float
sumMemory containers =
    List.sum (List.map (\container -> toFloat container.memory) (Dict.values containers)) / 1000


sumCPUShare : Configuration.Containers -> Int
sumCPUShare containers =
    List.sum (List.map (\container -> container.cpuShare) (Dict.values containers))


sumBandwidth : Configuration.Containers -> Int
sumBandwidth containers =
    List.sum (List.map (\container -> container.bandwidth) (Dict.values containers))


sumIoops : Configuration.Controller -> Configuration.Containers -> String
sumIoops controller containers =
    --     -- This feels like a lot of duplicated code
    --     --containersWithEBS =
    --       --  List.filter (\container -> container.useEBS == True) (Dict.values containers)
    --
    --     --containersWoEBS =
    --         --List.filter (\container -> container.useEBS == False) (Dict.values containers)
    --
    --
    --     --someUseEBS =
    --
    let
      otherSum =
        List.sum (List.map (\container -> container.ioops) (Dict.values containers))
      --List.length containersWithEBS > 0
    in
    -- if 0 == 0 then
    --     String.fromInt (List.length containersWithEBS) ++ " container(s) using EBS. Total: " ++ String.fromInt otherSum ++ "MiB/sec"

    --else
      String.fromInt (otherSum * controller.nominalPods) ++ " MiB/sec"
