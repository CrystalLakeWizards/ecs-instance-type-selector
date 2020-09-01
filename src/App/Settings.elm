module App.Settings exposing (Model, Msg(..), init, subscriptions, update, view)

import App.Constants exposing (instanceTypes)
import App.Util as Util
import Bootstrap.Card as Card
import Bootstrap.Card.Block as Block
import Bootstrap.Form as Form
import Bootstrap.Grid.Col as Col
import Html exposing (..)
import Html.Attributes exposing (..)
import Multiselect


type alias Model =
    { excludedInstances : Multiselect.Model
    , enableLiveResults : Bool
    }


init : Model
init =
    { excludedInstances = Multiselect.initModel instanceTypes "A"
    , enableLiveResults = True
    }



-- There's a better way to do this...


type Msg
    = UpdateExcludedInstances Multiselect.Msg
    | UpdateEnableLiveResults Bool


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateExcludedInstances instancesChangedMessage ->
            let
                ( newExcludedInstances, subCmd, _ ) =
                    Multiselect.update instancesChangedMessage model.excludedInstances
            in
            ( { model | excludedInstances = newExcludedInstances }, Cmd.map UpdateExcludedInstances subCmd )

        UpdateEnableLiveResults value ->
            ( { model | enableLiveResults = value }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.map UpdateExcludedInstances <| Multiselect.subscriptions model.excludedInstances


view : Model -> Html Msg
view model =
    Card.config []
        |> Card.header [] [ text "Global Settings" ]
        |> Card.block []
            [ Block.custom <|
                Form.form []
                    [ Form.row []
                        [ Form.colLabel [ Col.sm3 ] [ text "Excluded Instance Types" ]
                        , Form.col [ Col.sm9 ]
                            [ Html.map UpdateExcludedInstances <| Multiselect.view model.excludedInstances
                            , Form.help [] [ text "Exclude specific ECS instances. These will be ignored during the cluster optimization calculation." ]
                            ]
                        ]
                    , hr [] []
                    , Util.viewFormCheckbox "Enable live results" "If enabled, the results are immediately updated when the configuration is modified." model.enableLiveResults UpdateEnableLiveResults
                    ]
            ]
        |> Card.view