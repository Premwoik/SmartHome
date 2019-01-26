import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Html.Lazy
import Material
import Material.Icon as Icon
import Material.Slider as Slider
import Material.Scheme as Scheme
import Material.Card as Card
import Material.List as Lists
import Material.Layout as Layout
import Material.Button as Button
import Material.Textfield as Textfield
import Material.Toggles as Toggles
import Material.Color as Color
import Material.Options as Options exposing (css)
import Material.Typography as Typography
import Material.Elevation as Elevation
import Task

import RouteUrl as Routing
import Navigation
import Array exposing (Array)
import Dict exposing (Dict)
import Http
import Data.Dashboard exposing (DashboardShort, Dashboard)

import Page.Dashboard as Dashboard
import Page.Dashboard.Type as Dashboard
import Request
import Data.Id exposing (Id)
import Material.Helpers exposing (lift)
import Array
import Admin as Admin

import Phoenix.Socket
import Phoenix.Channel


-- MODE

type TabsType
    = DashboardType
    | AdminType
    | DefaultType


type alias Model =
    { mdl : Material.Model
    , dashboard : Dashboard.Model
    , admin : Admin.Model
    , tabsType : TabsType
    , selectedTab : Int
    , phxSocket : Phoenix.Socket.Socket Msg
    }

model : Model
model =
    { mdl = Material.model
    , dashboard = Dashboard.model
    , admin = Admin.model
    , tabsType = DefaultType
    , selectedTab = 0
    , phxSocket = Phoenix.Socket.init "ws://localhost:4000/socket/websocket"
          |> Phoenix.Socket.withDebug
          |> Phoenix.Socket.on "update" "dashboard:lobby" (Dashboard.ReceiveDashboardMessage >> DashboardMsg)
    }

-- UPDATE

type Msg
    = SelectTab Int
    | BackHome
    | AddressChange String
    | Mdl (Material.Msg Msg)
    | DashboardMsg Dashboard.Msg
    | AdminMsg Admin.Msg
    | PhoenixMsg (Phoenix.Socket.Msg Msg)
    | JoinChannel



update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        _ = Debug.log "MAIN_LOG: " msg
    in
    case msg of

        PhoenixMsg msg ->
           let
             ( phxSocket, phxCmd ) = Phoenix.Socket.update msg model.phxSocket
           in
             ( { model | phxSocket = phxSocket }
             , Cmd.map PhoenixMsg phxCmd
             )

        BackHome ->
            update (SelectTab 0) {model | tabsType = DefaultType}

        SelectTab k -> --TODO make this more beautiful
            case Array.get k (Array.fromList tabs) of
                Just (_, _, _, _, cmd_, type_, ch) ->
                    let
                       (phxSocLeave, phxCmdLeave) =
                           case Array.get model.selectedTab tabsChannel of
                               Just (Just ch_) ->
                                   Phoenix.Socket.leave ch_ model.phxSocket
                               _ ->
                                   (model.phxSocket, Cmd.none)

                       (phxSocJoin, phxCmdJoin) =
                           ch |> Maybe.map (\x -> join x) >> Maybe.withDefault (phxSocLeave, phxCmdLeave)

                       join ch_ = Phoenix.Socket.join (Phoenix.Channel.init ch_) phxSocLeave
                       phxMap x = Cmd.map PhoenixMsg x
                       getSocRef = ch |> Maybe.andThen (\x -> Dict.get x phxSocJoin.channels) |> Maybe.map .joinRef
                       updatePageModel =
                           case type_ of
                               DashboardType ->
                                    let
                                        dash = model.dashboard
                                        newDash = {dash | socketRef = getSocRef}
                                    in
                                    {model | dashboard = newDash}
                               _ ->
                                    model
                    in
                    case k == model.selectedTab of
                        True ->
                            ( model , Cmd.none)
                        False ->
                            ( { updatePageModel | selectedTab = k, tabsType = type_, phxSocket = phxSocJoin}
                            , Cmd.batch [phxMap phxCmdLeave, phxMap phxCmdJoin, cmd_ model]
                            )
                Nothing ->
                    ( model, Cmd.none )

        JoinChannel ->
            let
                channel =
                    Phoenix.Channel.init "dashboard:lobby"

                ( phxSocket, phxCmd ) =
                    Phoenix.Socket.join channel model.phxSocket
            in
                ( { model | phxSocket = phxSocket }
                , Cmd.map PhoenixMsg phxCmd
                )

        AddressChange tab ->
            let
                k = 0
            in
            update (SelectTab k) model

        Mdl action_ ->
            Material.update Mdl action_ model

        DashboardMsg a ->
            lift .dashboard (\m x -> {m | dashboard = x}) DashboardMsg Dashboard.update a model

        AdminMsg a ->
            lift .admin (\m x -> {m | admin = x}) AdminMsg Admin.update a model



--nameToNumber : String -> Model -> Int
--nameToNumber name =
--    let
--        _ = Debug.log "nameToNumber" name
--    in
--    .tabs >> Array.filter (\x -> (String.toLower x.name) == name) >> Array.map (\x -> x.number) >> Array.get 0 >> Maybe.withDefault -1



-- VIEW

view : Model -> Html Msg
view =
    Html.Lazy.lazy view_


view_ : Model -> Html Msg
view_ model =
    let
        top = (Array.get model.selectedTab tabsViews |> Maybe.withDefault e404) model
        selectedTab =
            case model.tabsType of
                DefaultType -> model.selectedTab
                DashboardType -> model.dashboard.selectedTab
                AdminType -> model.admin.selectedTab
        onSelectTab =
            case model.tabsType of
                DefaultType -> SelectTab
                DashboardType -> (\x ->DashboardMsg (Dashboard.SelectTab x))
                AdminType -> (\x -> AdminMsg (Admin.SelectTab x))

    in
      Scheme.top <|  Layout.render Mdl
            model.mdl
            [ Layout.selectedTab selectedTab
            , Layout.onSelectTab onSelectTab
            , Layout.fixedHeader
            , Layout.fixedDrawer
            , Layout.waterfall True
            ]
            { drawer = drawer model
            , header = header
            , main =
                [ (node "meta" [ name "viewport", content "width=device-width, initial-scale=1, maximum-scale=1, user-scalable=no" ] [])
                , top
                ]
            , tabs =
                    (  tabTitles model, [] )

            }

drawer : Model -> List (Html Msg)
drawer model =
    let
        weather =
            Options.div
                [ css  "height" "100px"
                , css  "width" "100px"
                , css "padding" "0 1rem 0 1rem"

                , css "margin-left" "3rem"
--                , css "background-color" "red"

                ] [img [src "../static/images/home.svg", width 100, height 100] []]
        liColor k = if model.selectedTab == k then css "color" "red" else Options.nop
    in
    [ Options.div
        [ css "margin" "5px"
        ]
        [ weather
        , Lists.ul []
            (List.map (\(k, x) -> Lists.li [Options.onClick (SelectTab k), liColor k] [Lists.content [] [x]]) mIndTabsTitles)
        ]
    ]

header : List (Html Msg)
header =
    [div [style
                          [ ( "float", "left" )
                          , ( "cursor", "default")
                          , ( "padding-left", "50px" )
                          ]
                          ]
        [ h5
            [ onClick BackHome
            ]
            [ text "SmartHome" ]
        ]]



tabs : List (String, String, (Model -> Html Msg), (Model -> Sub Msg), (Model -> Cmd Msg), TabsType, Maybe String)
tabs =
    [ ("Home", "home", (\_ -> div [] [text "HOME"]), (\_ -> Sub.none), (\_ -> Cmd.none), DefaultType, Nothing)
    , ( "Dashboard", "dashboard", .dashboard >> Dashboard.view >> Html.map DashboardMsg
      , .dashboard >> Dashboard.subs >> Sub.map DashboardMsg, .dashboard >> Dashboard.init >> Cmd.map DashboardMsg
      , DashboardType, Just "dashboard:lobby"
      )
    , ( "Management", "admin", .admin >> Admin.view >> Html.map AdminMsg
      , .admin >> Admin.subs >> Sub.map AdminMsg, .admin >> Admin.init >> Cmd.map AdminMsg, AdminType
      , Nothing
      )
--    , ("___ ", "", (\_ -> div [] []), (\_ -> Sub.none), (\_ -> Cmd.none), DefaultType)
    ]

tabTitles : Model -> List (Html a)
tabTitles model =
    case model.tabsType of
      DefaultType ->
          mTabsTitles
      AdminType ->
          Admin.tabsTitles |>  List.map (\x -> text x)
      DashboardType ->
          model |> .dashboard >> Dashboard.tabsTitles >> List.map (\x -> text x)

mTabsTitles : List (Html a)
mTabsTitles =
    tabs |> List.map (\(x, _, _, _, _, _, _) -> text x)

mIndTabsTitles : List (Int, Html a)
mIndTabsTitles =
    tabs |> List.indexedMap (\y (x, _, _, _, _, _, _) -> (y, text x))


tabsViews : Array (Model -> Html Msg)
tabsViews =
    tabs |> List.map (\(_, _, v, _, _, _, _) -> v) >> Array.fromList

tabsSubs : Array (Model -> Sub Msg)
tabsSubs =
    tabs |> List.map (\(_, _, _, s, _, _, _) -> s) >> Array.fromList

tabsUrls : Array String
tabsUrls =
    tabs |> List.map (\(_, u, _, _, _, _, _) -> u) >> Array.fromList

--tabsInit : Array (Model -> Cmd Msg)
--tabsInit =
--    tabs |> List.map (\(_, _, _, _, i, _) -> i) >> Array.fromList

--tabsType : Array TabsType
--tabsType =
--    tabs |> List.map (\(_, _, _, _, _, t) -> t) >> Array.fromList

tabsChannel : Array (Maybe String)
tabsChannel =
    tabs |> List.map (\(_,_,_,_,_,_,c) -> c) >> Array.fromList


e404 : Model -> Html Msg
e404 _ =
  div []
    [ Options.styled Html.h1
        [ Options.cs "mdl-typography--display-4"
        , Typography.center
        ]
        [ text "404" ]
    ]


-- ROUTING




urlOf : Model -> String
urlOf model =
    "#" ++ (Array.get model.selectedTab tabsUrls |> Maybe.withDefault "")

delta2url : Model -> Model -> Maybe Routing.UrlChange
delta2url model1 model2 =
    let
        _ = Debug.log "m1"
    in
    if model1.selectedTab /= model2.selectedTab then
        { entry = Routing.NewEntry
        , url = urlOf model2
        }
            |> Just
    else
        Nothing


location2messages : Navigation.Location -> List Msg
location2messages location =
    let
        _ = Debug.log "Location" location.hash
    in
    [ case String.dropLeft 1 location.hash of
        "" ->
            SelectTab 0
        x -> AddressChange x
    ]


init : Cmd Msg
init =
    Cmd.batch [Material.init Mdl] --, Task.succeed JoinChannel |> Task.perform identity]

subs : Model -> Sub Msg
subs model =
    Sub.batch
        [ Material.subscriptions Mdl model
        , Sub.map DashboardMsg (Dashboard.subs model.dashboard)
        , Phoenix.Socket.listen model.phxSocket PhoenixMsg
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ model |> .selectedTab >> (\x -> Array.get x tabsSubs) >> Maybe.map (\x -> x model) >> Maybe.withDefault Sub.none
        , Material.subscriptions Mdl model
        ]


main : Routing.RouteUrlProgram Never Model Msg
main =
    Routing.program
        { delta2url = delta2url
        , location2messages = location2messages
        , init =
            ( {model
                | mdl =
                    Layout.setTabsWidth 2124 model.mdl
                    {- elm gives us no wzy to measure the actual width of tabs. We
                       hardwire it. If you add a tab, remember to update this. Find the
                       new value using:
                       document.getElementsByClassName("mdl-layout__tab-bar")[0].scrollWidth
                    -}
              }
            , init
            )
        , view = view
        , subscriptions =
            \model -> subs model
        , update = update
        }
