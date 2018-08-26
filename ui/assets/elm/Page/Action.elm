module Page.Action exposing (..)

import Material
import Material.Helpers exposing (pure, effect)
import Material.Table as Table
import Material.Options as Options exposing (css)
import Material.Button as Button

import Html exposing (..)
import Html.Events exposing (onClick)
import Http
import Data.Action as Action exposing (Action)
import Data.Id as Id
import Page
import Request.Action
import Request
import Util exposing (replaceListElem)

-- INIT

init : Cmd Msg
init =
    Request.send Init Request.Action.getActions

-- MODEL

type alias Model =
    { mdl : Material.Model
    , actions : List Action
    }

model : Model
model =
    { mdl = Material.model
    ,  actions = []--[Action (Id.Id 1) True (Action.Function "function_1") [] []]
    }


-- UPDATE

type Msg
    = Mdl (Material.Msg Msg)
    | ToggleAction Action
    | Init (Result Http.Error (List Action))
    | Response (Result Http.Error String)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    let
        _ = Debug.log "debug Action Page: " msg
    in
    case msg of
        Mdl action_ ->
            Material.update Mdl action_ model

        Init (Ok actions) ->
            { model
            | actions = actions
            } |> pure
        Init (Err _) ->
            model |> pure

        Response _ ->
            model |> pure

        ToggleAction action ->
            let
                newAction = {action | state = not action.state}
            in
            { model
            | actions = replaceListElem model.actions newAction
            } |> effect (Request.send Response <| Request.Action.toggleAction action.id)


-- VIEW

view : Model -> Html Msg
view model =
    Page.body1
        [ Html.h1 [] [text "Akcje"]
        , tableView model
        , Html.br [] []
        , modifyBarView model
        ]


modifyBarView : Model -> Html Msg
modifyBarView model =
    Options.div
        [ css "background-color" "#cecece"
        ]
        [ Button.render Mdl [0,0] model.mdl [] [text "Add"]
        ]

tableView : Model -> Html Msg
tableView model =
    Table.table  [css "margin" "auto"]
        [ Table.thead []
            [ Table.tr []
                [ Table.th [css "text-align" "center"] [text "id"]
                , Table.th [css "text-align" "center"] [text "aktywator"]
                , Table.th [css "text-align" "center"] [text "funkcja"]
                , Table.th [css "text-align" "center"] [text "aktywny"]
                , Table.th [css "text-align" "center"] [text "parametry"]
                ]
            ]
        , Table.tbody [] (List.map (\x -> row x model.mdl) model.actions)
        ]


row : Action -> Material.Model -> Html Msg
row action mdl =
    Table.tr []
        [ Table.td [] [text << toString << Id.toInt <| action.id]
        , Table.td [] [text action.activator]
        , Table.td [] [text << Action.fnToString <| action.function]
        , Table.td [] [toggleButton action mdl]
        , Table.td [] [text << toString <| action.params]
        ]

toggleButton : Action -> Material.Model-> Html Msg
toggleButton action mdl=
    let
        nb = Id.toInt action.id
    in
    Button.render Mdl [nb] mdl
      [  Options.onClick (ToggleAction action)
      , Options.when action.state Button.colored
      ]
      [ text << toString <| action.state]