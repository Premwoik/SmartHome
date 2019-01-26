module Page.Dashboard exposing (..)

import Material
import Material.Card as Card
import Material.Color as Color
import Material.Helpers exposing (map1st, map2nd)
import Material.Options as Options exposing (css)
import Material.Typography as Typography
import Material.Elevation as Elevation
import Material.Icon as Icon
import Material.Button as Button
import Material.Slider as Slider
import Material.Menu as Menu
import Material.Toggles as Toggles
import Material.Snackbar as Snackbar
import Material.Grid as Grid

import Data.Dashboard as Dashboard exposing (Content, Dashboard, DashboardShort,getDashboard, getTabs, decodeUpdateNoti, updateContent, ShortContent)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)

import Page
import Request
import Http
import Data.Id as Id  exposing (Id)
import Json.Encode as Encode
import Json.Decode as Decode
import Window
import List.Split as LSplit
import Array exposing (Array)

import Data.Port as Port exposing (Port)
import Data.Light as Light exposing (Light)
import Data.Dimmer as Dimmer exposing (Dimmer)
import Data.Action as Action exposing (Action)
import Data.Sunblind as Sunblind exposing (Sunblind)
import Data.Task as Task exposing (Task)

import Json.Encode as Encode
import Json.Decode as Decode

import Page.Dashboard.Chart exposing (ch2, ch1, chart)
import Page.Dashboard.Card exposing (genCard)
import Page.Dashboard.Type exposing (..)




-- INIT
init : Model -> Cmd Msg
init model =
    if model.loaded then
        Cmd.none
    else
        Cmd.batch
            [ Request.send LoadTabs getTabs
            ]

subs : Model -> Sub Msg
subs model=
   Sub.batch [ Menu.subs Mdl model.mdl]

tabsTitles : Model -> List String
tabsTitles =
    .tabs >> Array.map (\x -> x.name) >> Array.toList

tabsUrls : Model -> Array String
tabsUrls =
    .tabs >> Array.map (\x -> String.toLower x.name)




model : Model
model =
    { data = Dashboard.empty
    , inProgress = Nothing
    , socketRef = Nothing
    , loaded = False
    , tabs = [] |> Array.fromList
    , selectedTab = 0
    , raise = -1
    , mdl = Material.model
    , snackbar = Snackbar.model
    , usedIndex = -1
    }




isInContent2 : ShortContent -> List ShortContent -> Cmd Msg
isInContent2 c d =
    case List.any (\x -> x == c) d of
        True ->
            case c.type_ of
                "dimmer" ->
                    Request.send ResponseDimmer (Dimmer.getView c.id)
                "light" ->
                    Request.send ResponseLight (Light.getView c.id)
                "port" ->
                    Request.send ResponsePort (Port.getView c.id)
                "action" ->
                    Request.send ResponseAction (Action.getView c.id)
                "sunblind" ->
                    Request.send ResponseSunblind (Sunblind.getView c.id)
                "task" ->
                    Request.send ResponseTask (Task.getView c.id)
                _ -> Cmd.none
        _ ->
            Cmd.none

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        skip = (model, Cmd.none)
        sRef = model.socketRef
        nomodel = \x -> (model, x)
        dash = model.data
        updateDashboard p = {model | data = p}
        updateContent_ x = updateDashboard {dash | content = updateContent x dash.content}
    in
    case msg of

        SelectTab k ->
            let
                send = .id >> Id.toInt >> getDashboard >> Request.send LoadDashboard
                loadDashboard = .tabs >> Array.get k >> Maybe.map send >> Maybe.withDefault Cmd.none
            in
            ( {model | selectedTab = k}, loadDashboard model )

        LoadTabs (Ok data) ->
            let
                tabs_ = Array.fromList <| List.sortBy .number data
                newModel = {model | tabs = tabs_, loaded = True}
            in
            update (SelectTab 0) newModel
        LoadTabs (Err err) ->
            ({model | tabs = Array.fromList []}, Cmd.none)

        LoadDashboard (Ok d) ->
            ({model | data = Dashboard.makeShortContent d}, Cmd.none)
        LoadDashboard (Err _) ->
            ({model | data = Dashboard.empty}, Cmd.none)

        Raise k ->
            ({model | raise = k}, Cmd.none)

        PortToggle p ->
            p |> Port.toggle sRef >> Request.send ResponsePort >> nomodel

        LightToggle l->
            l |> Light.toggle sRef >> Request.send ResponseLight >> nomodel

        SunblindToggle s->
            s |> Sunblind.click sRef >> Request.send ResponseSunblind >> nomodel

        DimmerToggle d->
            d |> Dimmer.toggle sRef >> Request.send ResponseDimmer >> nomodel

        SetDimmerFill d f ->
            d |> Dimmer.setFill f sRef >> Request.send ResponseDimmer >> nomodel

        DimLightToggle d l->
            l |> Dimmer.toggleLight sRef >> Request.send ResponseDimmer >> nomodel

        SunblindManualToggle s ->
            s |> Sunblind.toggleManual sRef >> Request.send ResponseSunblind >> nomodel

        ActionToggle a ->
            a |> Action.toggle sRef >> Request.send ResponseAction >> nomodel

        TaskToggle t ->
            t |> Task.toggle sRef >> Request.send ResponseTask >> nomodel

        ResponseDimmer (Ok d) ->
            (updateContent_ (Dashboard.Dimmer d), Cmd.none)
--            (updateContent model.raise (Dashboard.Dimmer d), Cmd.none)
        ResponseDimmer (Err _) ->
            ({model | data = Dashboard.empty}, Cmd.none)
        ResponseLight (Ok l) ->
            (updateContent_ (Dashboard.Light l), Cmd.none)
--                (updateContent model.raise (Dashboard.Light l), Cmd.none)
        ResponseLight (Err _) ->
            ({model | data = Dashboard.empty}, Cmd.none)
        ResponsePort(Ok p) ->
            (updateContent_ (Dashboard.Port p), Cmd.none)
--            (updateContent model.raise (Dashboard.Port p), Cmd.none)
        ResponsePort(Err _) ->
            ({model | data = Dashboard.empty}, Cmd.none)
        ResponseSunblind(Ok s) ->
            (updateContent_ (Dashboard.Sunblind s), Cmd.none)
--                (updateContent model.raise (Dashboard.Sunblind s), Cmd.none)
        ResponseSunblind (Err _) ->
            ({model | data = Dashboard.empty}, Cmd.none)
        ResponseAction(Ok a) ->
            (updateContent_ (Dashboard.Action a), Cmd.none)
--                (updateContent model.raise (Dashboard.Action a), Cmd.none)
        ResponseAction (Err _) ->
            ({model | data = Dashboard.empty}, Cmd.none)

        ResponseTask (Ok t) ->
            (updateContent_ (Dashboard.Task t), Cmd.none)
--            (updateContent model.raise (Dashboard.Task t), Cmd.none)
        ResponseTask (Err _) ->
            ({model | data = Dashboard.empty}, Cmd.none)

        LightEdit l ->
            skip
        DimmerEdit d ->
            skip
        ActionEdit a ->
            skip
        SunblindEdit s ->
            skip
        PortEdit p ->
            skip
        TaskEdit t ->
            skip

        ReceiveDashboardMessage raw ->
            let
                _ = Debug.log "Reveive" raw
                (model_, cmd_ ) = update (ShowToast "Update cards") model
            in

            case decodeUpdateNoti raw of
                Ok l ->
                    (model_, Cmd.batch (cmd_ ::(List.map (\x -> isInContent2 x model.data.shortContent) l)))

                Err error ->
                    let
                        _ = Debug.log "ERROR" error
                    in
                    ( model, Cmd.none)
        ShowToast msg ->
            Snackbar.add (Snackbar.toast 1 <| msg) model.snackbar
                |> map1st (\s -> { model | snackbar = s })
                |> map2nd (Cmd.map Snackbar)

        Skip ->
            skip


        Snackbar msg_ ->
            Snackbar.update msg_ model.snackbar
                |> map1st (\s -> { model | snackbar = s })
                |> map2nd (Cmd.map Snackbar)

        Mdl action_ ->
            Material.update Mdl action_ model


view : Model -> Html Msg
view model =
    let
        content =
            Html.main_
                [ align "center"
                , style [("margin", "auto"), ("padding", "20px"), ("max-width", "1300px")]
                ]
                [ Grid.grid []
                    [ Grid.cell [Grid.size Grid.Phone 4, Grid.size Grid.Tablet 8, Grid.size Grid.Desktop 6] [chart model "Temperatura salon" ch1 True False False]
                    , Grid.cell [Grid.size Grid.Phone 4, Grid.size Grid.Tablet 8, Grid.size Grid.Desktop 6] [chart model "Użycie energii dom" ch2 False True False]
                    ]
                , Grid.grid []
                    (genCells model)

                , Snackbar.view model.snackbar |> Html.map Snackbar
                ]
    in
    Page.body1 "Panel sterowania" "" (div [] []) [] [content]

-- View



split : Int -> Int -> List a -> List (List a)
split i r list =
  let
      n = if r > 0 then i+1 else i
  in
  case List.take n list of
    [] -> []
    listHead -> listHead :: split i (r-1) (List.drop n list)


genCells : Model -> List (Grid.Cell Msg)
genCells model =
    let
        l_ = model |> .data >> .content
        rest = (List.length l_) % 4
        elems = (List.length l_) // 4
        cards = split elems rest l_
        cellBody d = List.indexedMap (\i x -> genCard model.mdl i x model.raise) d
    in
    (List.map(\x -> Grid.cell [Grid.size Grid.Phone 4, Grid.size Grid.Tablet 4, Grid.size Grid.Desktop 3] (cellBody x)) cards)








-- Dashboard API

--unwrapDashboard : Model -> (Dashboard -> prop) -> prop -> prop
--unwrapDashboard model prop default=
--    model |> .data >> Maybe.map prop >> Maybe.withDefault default




