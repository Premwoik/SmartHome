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

import Data.Dashboard as Dashboard exposing (Content, Dashboard, DashboardShort,getDashboard, getTabs)
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


import Json.Encode as Encode
import Json.Decode as Decode
import Task

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


type alias Model =
    { data : Dashboard
    , inProgress : Maybe Dashboard.Content
    , loaded : Bool
    , tabs : Array DashboardShort
    , selectedTab : Int
    , raise : Int
    , mdl : Material.Model
    , snackbar : Snackbar.Model Int
    , usedIndex : Int -- TODO change to dictionary
    }

model : Model
model =
    { data = Dashboard.empty
    , inProgress = Nothing
    , loaded = False
    , tabs = [] |> Array.fromList
    , selectedTab = 0
    , raise = -1
    , mdl = Material.model
    , snackbar = Snackbar.model
    , usedIndex = -1
    }


type Msg
    = Mdl (Material.Msg Msg)
    | Snackbar (Snackbar.Msg Int)
    | SelectTab Int
    | LoadTabs (Result Http.Error (List DashboardShort))
    | LoadDashboard (Result Http.Error Dashboard)
    | Raise Int
    | PortToggle Port
    | LightToggle Light
    | SunblindToggle Sunblind
    | SetDimmerFill Dimmer Float
    | DimLightToggle Dimmer Dimmer.Light
    | DimmerToggle Dimmer
    | ActionToggle Action
    | PortEdit Port
    | DimmerEdit Dimmer
    | SunblindEdit Sunblind
    | ActionEdit Action
    | LightEdit Light
    | Skip
    | ResponseDimmer (Result Http.Error Dimmer)
    | ResponseLight (Result Http.Error Light)
    | ResponseSunblind (Result Http.Error Sunblind)
    | ResponsePort (Result Http.Error Port)
    | ResponseAction (Result Http.Error Action)
    | SunblindManualToggle Sunblind
    | ReceiveDashboardMessage Decode.Value



update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        skip = (model, Cmd.none)
        nomodel = \x -> (model, x)
        dash = model.data
        updateDashboard p = {model | data = p}
        updateContent i x = updateDashboard {dash | content = Array.set i x dash.content}
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
            ({model | data = d}, Cmd.none)
        LoadDashboard (Err _) ->
            ({model | data = Dashboard.empty}, Cmd.none)

        Raise k ->
            ({model | raise = k}, Cmd.none)

        PortToggle p ->
            p |> Port.toggle >> Request.send ResponsePort >> nomodel

        LightToggle l->
            l |> Light.toggle >> Request.send ResponseLight >> nomodel

        SunblindToggle s->
            s |> Sunblind.click >> Request.send ResponseSunblind >> nomodel

        DimmerToggle d->
            d |> Dimmer.toggle >> Request.send ResponseDimmer >> nomodel

        SetDimmerFill d f ->
            d |> Dimmer.setFill f >> Request.send ResponseDimmer >> nomodel

        DimLightToggle d l->
            l |> Dimmer.toggleLight >> Request.send ResponseDimmer >> nomodel

        SunblindManualToggle s ->
            s |> Sunblind.toggleManual >> Request.send ResponseSunblind >> nomodel

        ActionToggle a ->
            a |> Action.toggle >> Request.send ResponseAction >> nomodel

        ResponseDimmer (Ok d) ->
            (updateContent model.raise (Dashboard.Dimmer d), Cmd.none)
        ResponseDimmer (Err _) ->
            ({model | data = Dashboard.empty}, Cmd.none)
        ResponseLight (Ok l) ->
                (updateContent model.raise (Dashboard.Light l), Cmd.none)
        ResponseLight (Err _) ->
            ({model | data = Dashboard.empty}, Cmd.none)
        ResponsePort(Ok p) ->
            (updateContent model.raise (Dashboard.Port p), Cmd.none)
        ResponsePort(Err _) ->
            ({model | data = Dashboard.empty}, Cmd.none)
        ResponseSunblind(Ok s) ->
                (updateContent model.raise (Dashboard.Sunblind s), Cmd.none)
        ResponseSunblind (Err _) ->
            ({model | data = Dashboard.empty}, Cmd.none)
        ResponseAction(Ok a) ->
                (updateContent model.raise (Dashboard.Action a), Cmd.none)
        ResponseAction (Err _) ->
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

        ReceiveDashboardMessage raw ->
            let
                _ = Debug.log "Reveive" raw
            in
            Snackbar.add (Snackbar.toast 1 <| "Toast message #") model.snackbar
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
        l_ = model |> .data >> .content >> Array.toIndexedList
        rest = (List.length l_) % 4
        elems = (List.length l_) // 4
        cards = split elems rest l_
        cellBody d = List.map (\(i, x) -> genCard model.mdl i x model.raise) d
    in
    (List.map(\x -> Grid.cell [Grid.size Grid.Phone 4, Grid.size Grid.Tablet 4, Grid.size Grid.Desktop 3] (cellBody x)) cards)



genCard : Material.Model -> Int -> Content -> Int -> Html Msg
genCard mdl i content raise =
    let
        _ = ""

        checkmark x =
            if x then
              Icon.view "check" [ css "width" "40px" ]
            else
              Options.span [ css "width" "40px" ] []

    in
    case content of
        Dashboard.Dimmer d ->
            card [] "sciemniacz" d.name i raise (dimmerCard mdl d i raise) (DimmerEdit d)
        Dashboard.Sunblind s ->
            card [] "roleta" s.name i raise (sunblindCard mdl s) (SunblindEdit s)
        Dashboard.Light l ->
            card [] "światło" l.name i raise (lightCard mdl l) (LightEdit l)
        Dashboard.Action a ->
            card [] "akcja" a.name i raise (actionCard mdl a) (ActionEdit a)
        Dashboard.Port p ->
            card [] "port" p.name i raise (portCard mdl p) (PortEdit p)



actionCard : Material.Model -> Action -> Card.Block Msg
actionCard mdl action =
    let
        _ = ""
    in
    Card.actions []
        [ Options.div
            cardActionCss
            [ Options.span [Typography.title] [text "Funkcja: "]
            , Options.span [Typography.body2, css "padding-left" "1rem"] [text action.function]
            , Card.subhead
               [ css "display" "flex"
               , css "align-items" "center"
               , css "padding" ".3rem 2.5rem"
               ]
               [ Button.render Mdl [0] mdl
                       [ Button.fab
                       , Options.css "align" "center"
                       , Options.onClick (ActionToggle action)
                       , Options.when action.state Button.colored
                       ]
                       [ Icon.i "star"]
               ]

            ]
        ]


dimmerCard : Material.Model -> Dimmer-> Int -> Int -> Card.Block Msg
dimmerCard mdl dimmer i raise=
    let
        cell =
            css "width" "64px"
        color =
          Color.color Color.Amber Color.S500
        icon =
            "wb_sunny"

        lightRow light=
              Card.subhead
                    [ css "display" "flex"
                    , css "align-items" "center"
                    , css "padding" ".3rem 2rem"
                    ]
                    [ Options.span [ cell ] [ text light.name ]
                    , Options.span [ cell, css "text-align" "center" ]
                        [ Icon.view icon [ Color.text color, Icon.size18 ] ]
                    , Options.span [ cell, css "text-align" "right" ]
                        [ Button.render Mdl [0] mdl
                             [ Button.fab
                             , Options.onClick (DimLightToggle dimmer light)
                             , Options.when light.state Button.colored
                             , Button.icon
--                             , state
                             ]
                             [ Icon.i "highlight"]
                        ]
                    ]

        renderLights =
--            if i == raise then
               Options.div
                [ css "padding-top" "2rem"
                , css "margin" "auto"
                ]
                ( Options.span
                     [ Typography.title
                     , Color.text Color.primary
                     , css "margin-bottom" "3rem"
                     ]
                     [ text "Światła:"]
                :: List.map lightRow dimmer.lights)
    in
    Card.actions []
        [ Options.div
             cardActionCss
             [ Card.subhead
                 [ css "display" "flex"
                 , css "align-items" "center"
                 , css "padding" ".3rem 2.5rem"
                 ]
                 [ Button.render Mdl [0] mdl
                         [ Button.fab
                         , Options.css "align" "center"
                         , Options.onClick (DimmerToggle dimmer)
                         , Options.when (dimmer.fill > 0) Button.colored
                         ]
                         [ Icon.i "highlight"]
                 ]
             , Slider.view
                [ Slider.step 25
                , Slider.value dimmer.fill
                , Slider.min 25
                , Slider.max 100
                , css "margin-top" "2rem"
                , Slider.onChange (SetDimmerFill dimmer)
--                        , (Options.when (not(Light.isOn light)) Slider.disabled)
                ]

             , renderLights

             ]
        ]


lightCard : Material.Model -> Light -> Card.Block Msg
lightCard mdl light =
     Card.actions
            []
            [ Options.div
                  cardActionCss
                  [ Card.subhead
                          [ css "display" "flex"
                          , css "align-items" "center"
                          , css "padding" ".3rem 2.5rem"
                          ]
                          [
                            Button.render Mdl [0] mdl
                                    [ Options.onClick (LightToggle light)
                                    , Button.fab
                                    , Options.when (light.state == True) Button.colored
                                    ]
                                    [ Icon.i "highlight"]
                          ]
                  ]
            ]


sunblindCard : Material.Model -> Sunblind -> Card.Block Msg
sunblindCard mdl sunblind =
    let

        manual =
            sunblind.state == Sunblind.Position
        icon =
            case sunblind.state of
                Sunblind.Position -> "loop"
                Sunblind.InMove -> "block"
                Sunblind.Open -> "bookmark_border"
                Sunblind.Close -> "bookmark"

    in
    Card.actions
        []
        [ Options.div cardActionCss
            [ Card.subhead
                [ css "display" "flex"
                , css "justify-content" "space-between"
                , css "align-items" "center"
                , css "padding" ".3rem 2.5rem"
                ]
                [ Button.render Mdl [0] mdl
                    [ Options.onClick (SunblindToggle sunblind)
                    , Button.fab
                    , Color.background (Color.color Color.Yellow Color.S300)
                    ]
                    [ Icon.i icon]
                ]
            , Toggles.switch Mdl [0] model.mdl
                [ Options.onToggle (SunblindManualToggle sunblind)
                , Toggles.ripple
                , Toggles.value manual
                ]
                [ text "Manual" ]
            ]
        ]

portCard : Material.Model -> Port -> Card.Block Msg
portCard mdl port_ =
    let
        _ = ""
        icon = if port_.state == True then "power" else "power_off"

    in
    Card.actions
        []
        [    Options.div
                         cardActionCss
                          [ Card.subhead
                                  [ css "display" "flex"
                                  , css "justify-content" "space-between"
                                  , css "align-items" "center"
                                  , css "padding" ".3rem 2.5rem"
                                  ]
                                  [
                                     Button.render Mdl [0] mdl
                                                 [ Options.onClick (PortToggle port_)
                                                 , Button.fab
                                                 , Options.when (port_.state == True) Button.colored
                                                 ]
                                                 [ Icon.i icon]
                                  ]
                          ]
        ]

card : List (Options.Style Msg) -> String -> String -> Int -> Int -> (Card.Block Msg) -> Msg ->  Html Msg
card props type_ name k raised action menuMsg=
    Card.view
           ([
              css "width" "100%"
            , css "margin-bottom" "1rem"
            , css "object-fit" "contain"
            , css "padding-bottom" "1rem"
            , dynamic k raised
            , Options.onClick Skip
            ] ++ props)
            [ Card.menu [] [
                  Button.render Mdl [0, k] model.mdl
                                          [ Button.icon
                                          , Options.onClick menuMsg
                                          , Button.ripple
                                          ]
                                          [ Icon.i "edit" ]
                  ]
            , Card.title
                [ css "padding" "1rem 1rem 0 1rem"]
                [ Card.head [] [text type_]
            , Options.div
                        [ css "padding" "0rem 2rem 0 2rem" ]
                        [ Options.span
                            [ Typography.display2
                            , Color.text Color.primary
                            ]
                            [ text name ]
                        ]
            ]
            , action
            ]

cardActionCss =
    [ css "display" "flex"
    , css "flex-direction" "column"
--    , css "margin-top" "20rem"
    ]

dynamic : Int -> Int -> Options.Style Msg
dynamic k raised =
  [ if k == raised then Elevation.e8 else Elevation.e2
  , Elevation.transition 250
  , Options.onMouseEnter (Raise k)
  , Options.onMouseLeave (Raise -1)
--  , Options.onClick showcode
  ] |> Options.many



-- Dashboard API

--unwrapDashboard : Model -> (Dashboard -> prop) -> prop -> prop
--unwrapDashboard model prop default=
--    model |> .data >> Maybe.map prop >> Maybe.withDefault default




