module Page.Page exposing (..)

import Material
import Material.Card as Card
import Material.Color as Color
import Material.Options as Options exposing (css)
import Material.Typography as Typography
import Material.Elevation as Elevation
import Material.Icon as Icon
import Material.Button as Button
import Material.Slider as Slider
import Material.Menu as Menu
import Material.Toggles as Toggles



import Data.Page as Page exposing (Page, PageShort, Content)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)




import Request
import Http
import Data.Id as Id  exposing (Id)
import Json.Encode as Encode
import Json.Decode as Decode
import Window
import List.Split as LSplit
import Array

import Data.Port as Port exposing (Port)
import Data.Light as Light exposing (Light)
import Data.Dimmer as Dimmer exposing (Dimmer)
import Data.Action as Action exposing (Action)
import Data.Sunblind as Sunblind exposing (Sunblind)


import Phoenix.Socket
import Phoenix.Channel
import Phoenix.Push
import Json.Encode as Encode
import Json.Decode as Decode
import Task

-- INIT
init : PageShort -> Cmd Msg
init short =
    Cmd.batch
        [ Request.send LoadPage (getPage (Id.toInt short.id))
        , Task.succeed JoinChannel |> Task.perform identity
        ]

subs : Model -> Sub Msg
subs model=
   Sub.batch [ Menu.subs Mdl model.mdl, Phoenix.Socket.listen model.phxSocket PhoenixMsg]

type alias Model =
    { data : Page
    , raise : Int
    , mdl : Material.Model
    , colNumber : Int
    , phxSocket : Phoenix.Socket.Socket Msg
    }

model =
    { data = Page.empty
    , raise = -1
    , mdl = Material.model
    , colNumber = 1
    , phxSocket = Phoenix.Socket.init "ws://localhost:4000/socket/websocket"
        |> Phoenix.Socket.withDebug
        |> Phoenix.Socket.on "new:msg" "dashboard1:lobby" ReceiveChatMessage
    }


type Msg
    = Mdl (Material.Msg Msg)
    | PhoenixMsg (Phoenix.Socket.Msg Msg)
    | WindowResized Window.Size
    | LoadPage (Result Http.Error Page)
    | Raise Int
    | PortToggle Port
    | LightToggle Light
    | SunblindToggle Sunblind
    | SetLightFill Light Float
    | SetDimmerFill Dimmer Float
    | DimLightToggle Dimmer Dimmer.Light
    | DimmerToggle Dimmer
    | ActionToggle Action
    | Skip
    | ResponseDimmer (Result Http.Error Dimmer)
    | ResponseLight (Result Http.Error Light)
    | ResponseSunblind (Result Http.Error Sunblind)
    | ResponsePort (Result Http.Error Port)
    | ResponseAction (Result Http.Error Action)
    | SunblindManualToggle Sunblind
    | ReceiveChatMessage Encode.Value
    | JoinChannel
    | LeaveChannel


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        skip = (model, Cmd.none)
        nomodel = \x -> (model, x)
        page = model.data
        updatePage p = {model | data = p}
        updateContent i x = updatePage {page | content = Array.set i x page.content}
        _ = Debug.log "Page - update - msg - log" (toString msg)
    in
    case msg of
        LoadPage (Ok page) ->
            ({model | data = page}, Cmd.none)
        LoadPage (Err _) ->
            ({model | data = Page.empty}, Cmd.none)
        Raise k ->
            ({model | raise = k}, Cmd.none)
        WindowResized size ->
            let
                _ = Debug.log "WindowResized" (toString size)
            in
            (model, Cmd.none)
        PortToggle p ->
            p |> Port.toggle >> Request.send ResponsePort >> nomodel

        LightToggle l->
            l |> Light.toggle >> Request.send ResponseLight >> nomodel

        SunblindToggle s->
            s |> Sunblind.click >> Request.send ResponseSunblind >> nomodel

        DimmerToggle d->
            d |> Dimmer.toggle >> Request.send ResponseDimmer >> nomodel

        SetLightFill l f->
            l |> Light.setFill f >> Request.send ResponseLight >> nomodel

        SetDimmerFill d f ->
            d |> Dimmer.setFill f >> Request.send ResponseDimmer >> nomodel

        DimLightToggle d l->
            l |> Dimmer.toggleLight >> Request.send ResponseDimmer >> nomodel

        SunblindManualToggle s ->
            s |> Sunblind.toggleManual >> Request.send ResponseSunblind >> nomodel

        ActionToggle a ->
            a |> Action.toggle >> Request.send ResponseAction >> nomodel

        ResponseDimmer (Ok d) ->
            (updateContent d.order (Page.Dimmer d), Cmd.none)
        ResponseDimmer (Err _) ->
            ({model | data = Page.empty}, Cmd.none)
        ResponseLight (Ok l) ->
                (updateContent l.order (Page.Light l), Cmd.none)
        ResponseLight (Err _) ->
            ({model | data = Page.empty}, Cmd.none)
        ResponsePort(Ok p) ->
            (updateContent p.order (Page.Port p), Cmd.none)
        ResponsePort(Err _) ->
            ({model | data = Page.empty}, Cmd.none)
        ResponseSunblind(Ok s) ->
                (updateContent s.order (Page.Sunblind s), Cmd.none)
        ResponseSunblind (Err _) ->
            ({model | data = Page.empty}, Cmd.none)
        ResponseAction(Ok a) ->
                (updateContent a.order (Page.Action a), Cmd.none)
        ResponseAction (Err _) ->
            ({model | data = Page.empty}, Cmd.none)

        Skip ->
            skip
        Mdl action_ ->
            Material.update Mdl action_ model

        ReceiveChatMessage raw ->
            case Decode.decodeValue Decode.string raw of
                Ok msg ->
                    let
                        _ = Debug.log "ChatMessage text: " msg
                    in
                    skip
                _ ->
                    skip
        JoinChannel ->
                    let
                        channel =
                            Phoenix.Channel.init "dashboard1:lobby"

                        ( phxSocket, phxCmd ) =
                            Phoenix.Socket.join channel model.phxSocket
                    in
                        ( { model | phxSocket = phxSocket }
                        , Cmd.map PhoenixMsg phxCmd
                        )

        LeaveChannel ->
            let
                ( phxSocket, phxCmd ) =
                    Phoenix.Socket.leave "dashboard1:lobby" model.phxSocket
            in
                ( { model | phxSocket = phxSocket }
                , Cmd.map PhoenixMsg phxCmd
                )

        PhoenixMsg msg ->
           let
             ( phxSocket, phxCmd ) = Phoenix.Socket.update msg model.phxSocket
           in
             ( { model | phxSocket = phxSocket }
             , Cmd.map PhoenixMsg phxCmd
             )

view : Model -> Html Msg
view model =
    Html.main_
        [ align "center"
        , style [("margin", "auto"), ("padding", "20px")]
        ]
        [ (node "meta" [ name "viewport", content "width=device-width, initial-scale=1, maximum-scale=1, user-scalable=no" ] [])
        , genCols model
        ]

-- View

split : Int -> List a -> List (List a)
split i list =
  case List.take i list of
    [] -> []
    listHead -> listHead :: split i (List.drop i list)


genCols : Model -> Html Msg
genCols model =
    let
        cards = model |> .data >> .content >> Array.toList
        colNumber = "auto"
        colWidth = "300px"
        colGap = "50px"
    in
    Options.div
        [ css "class" "col"
        , css "display" "grid"
        , css "grid-gap" "10px"
        , css "grid-template-columns" "repeat(auto-fill, minmax(260px, 1fr))"
        , css "grid-auto-rows" "260px"
        , css "overflow" "visible"
        ]
        (List.map (\x -> genCard model.mdl x model.raise) cards)

genCard : Material.Model -> Content -> Int -> Html Msg
genCard mdl content raise =
    let
        _ = ""
        menu num=
            Card.menu [] [
            Menu.render Mdl [num] mdl
              [ Menu.bottomRight]
              [ Menu.item
                  [ Menu.onSelect Skip ]
                  [ text "English (US)" ]

              ]]
    in
    case content of
        Page.Dimmer dim ->
            let
                props =
                    [Options.when (dim.order == raise) (css "grid-row" "span 2")]
            in
            card props "sciemniacz" dim.name dim.order raise (dimmerCard mdl dim raise) (menu dim.order)
        Page.Sunblind sun ->
            card [] "roleta" sun.name sun.order raise (sunblindCard mdl sun) (menu sun.order)
        Page.Light light ->
            card [] "światło" light.name light.order raise (lightCard mdl light) (menu light.order)
        Page.Action action ->
            card [] "akcja" action.name action.order raise (actionCard mdl action) (menu action.order)
        Page.Port port_ ->
            card [] "port" port_.name port_.order raise (portCard mdl port_) (menu port_.order)



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


dimmerCard : Material.Model -> Dimmer-> Int -> Card.Block Msg
dimmerCard mdl dimmer raise=
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
                             , Button.icon
--                             , state
                             ]
                             [ Icon.i "highlight"]
                        ]
                    ]

        renderLights =
            if dimmer.order == raise then
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
            else
                Options.div [] []


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
--                         , state
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
    let
        fill = Light.unpackFill light
        slider =
            Slider.view
              [ Slider.step 25
              , Slider.value fill
              , Slider.min 25
              , Slider.max 100
              , Slider.onChange (SetLightFill light)
              , css "margin-top" "2rem"
              , Options.when (not light.state) Slider.disabled
              ]
        maybeDimmered = if light.fill == Nothing then div [] [] else slider
    in
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
                                    , Options.when light.state Button.colored
                                    ]
                                    [ Icon.i "highlight"]
                          ]
                  ]
            , maybeDimmered
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
        [ Options.div
              cardActionCss

              [ Toggles.switch Mdl [0] model.mdl
                    [ Options.onToggle (SunblindManualToggle sunblind)
                    , Toggles.ripple
                    , Toggles.value manual
                    ]
                    [ text "Manual" ]


              , Card.subhead
                      [ css "display" "flex"
                      , css "justify-content" "space-between"
                      , css "align-items" "center"
                      , css "padding" ".3rem 2.5rem"
                      ]
                      [
                         Button.render Mdl [0] mdl
                                 [ Options.onClick (SunblindToggle sunblind)
                                 , Button.fab
                                 , Color.background (Color.color Color.Yellow Color.S300)
                                 ]
                                 [ Icon.i icon]
                      ]
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

card : List (Options.Style Msg) -> String -> String -> Int -> Int -> (Card.Block Msg) -> (Card.Block Msg) ->  Html Msg
card props type_ name k raised action menu=
    Card.view
           ([
              css "width" "100%"
            , css "height" "100%"
            , css "object-fit" "cover"
            , dynamic k raised
            , Options.onClick Skip
            ] ++ props)
            [ Card.title
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
            , menu
            ]

cardActionCss =
    [ css "display" "flex"
    , css "flex-direction" "column"
    , css "margin-top" "2rem"
    ]

dynamic : Int -> Int -> Options.Style Msg
dynamic k raised =
  [ if k == raised then Elevation.e8 else Elevation.e2
  , Elevation.transition 250
  , Options.onMouseEnter (Raise k)
  , Options.onMouseLeave (Raise -1)
--  , Options.onClick showcode
  ] |> Options.many



-- PAGE API

--unwrapPage : Model -> (Page -> prop) -> prop -> prop
--unwrapPage model prop default=
--    model |> .data >> Maybe.map prop >> Maybe.withDefault default


getTabs : Http.Request (List PageShort)
getTabs =
    let
        url_ = Request.url ++ "pages/short"
        decoder =  Decode.list Page.decoderShort
    in
    Http.get url_ decoder


getPage : Int -> Http.Request Page
getPage id =
  let
        url_ = Request.url ++ "pages/view/" ++ (toString id)

        decoder = Page.decoder
    in
    Http.get url_ decoder


