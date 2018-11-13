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

import RouteUrl as Routing
import Navigation
import Array exposing (Array)
import Dict exposing (Dict)
import Page.Action
import Http
import Data.Page exposing (PageShort, Page)
import Page.Page as Page
import Request
import Request.Room
import Data.Id exposing (Id)
import Material.Helpers exposing (lift)
import Array

-- MODEL

type alias Model =
    { mdl : Material.Model
    , page : Page.Model
    , selectedTab : Int
    , tabs : Array PageShort
    }

model : Model
model =
    { mdl = Material.model
    , selectedTab = 1
    , page = Page.model
    , tabs = [] |> Array.fromList
    }

-- UPDATE

type Msg
    = SelectTab Int
    | AddressChange String
    | LoadTabs (Result Http.Error (List PageShort))
    | Mdl (Material.Msg Msg)
    | PageMsg Page.Msg



update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of

        LoadTabs (Ok data) ->
            let
                tabs_ = Array.fromList <| List.sortBy .number data
            in
            ({model | tabs = tabs_}, Cmd.none)
        LoadTabs (Err err) ->
            ({model | tabs = Array.fromList []}, Cmd.none)

        SelectTab k ->
            ( { model | selectedTab = k }, switchTab k (Page.init) model)

        AddressChange tab ->
            ({model | selectedTab = nameToNumber tab model}, Cmd.none)

        Mdl action_ ->
            Material.update Mdl action_ model

        PageMsg a ->
            lift .page (\m x -> {m | page = x}) PageMsg Page.update a model


switchTab : Int -> (PageShort -> Cmd Page.Msg) -> Model -> Cmd Msg
switchTab k just =
    .tabs >> Array.get k >> Maybe.map (Cmd.map PageMsg << just) >> Maybe.withDefault (Cmd.none)

nameToNumber : String -> Model -> Int
nameToNumber name =
    .tabs >> Array.filter (\x -> x.name == name) >> Array.map (\x -> x.number) >> Array.get 0 >> Maybe.withDefault -1


-- VIEW

view : Model -> Html Msg
view =
    Html.Lazy.lazy view_


view_ : Model -> Html Msg
view_ model =
    let
        top = .page >> Page.view >> Html.map PageMsg <| model
--            (Array.get model.selectedTab tabViews |> Maybe.withDefault e404) model
    in
      Scheme.top <|  Layout.render Mdl
            model.mdl
            [ Layout.selectedTab model.selectedTab
            , Layout.onSelectTab SelectTab
            , Layout.fixedHeader
            , Layout.scrolling
            ]
            { drawer = []
            , header = header
            , main = [top]
            , tabs =
                    (  tabTitles model, [] )

            }


header : List (Html Msg)
header =
    [div []
        [ h5
            [ style
                [ ( "float", "left" )
                , ( "padding-left", "40px" )
                ]
            ]
            [ text "easyHome" ]
        ]]

tabTitles : Model -> List (Html a)
tabTitles model =
      Array.map (\x -> text x.name) >> Array.toList <| model.tabs


--e404 : Model -> Html Msg
--e404 _ =
--    div
--        []
--        [ Options.styled Html.h1
--            [ Options.cs "mdl-typography--display-4"
--            , Typography.center
--            ]
--            [ text "404" ]
--        ]


-- ROUTING




urlOf : Model -> String
urlOf model =
    let
        short = case Array.get model.selectedTab model.tabs of
                    Nothing -> ""
                    Just val -> val.name |> String.toLower
    in
    "#" ++ short

delta2url : Model -> Model -> Maybe Routing.UrlChange
delta2url model1 model2 =
    if model1.selectedTab /= model2.selectedTab then
        { entry = Routing.NewEntry
        , url = urlOf model2
        }
            |> Just
    else
        Nothing


location2messages : Navigation.Location -> List Msg
location2messages location =
    [ case String.dropLeft 1 location.hash of
        "" ->
            SelectTab 0
        x -> AddressChange x
    ]


init : Cmd Msg
init =
    Cmd.batch [Material.init Mdl, Request.send LoadTabs Page.getTabs]

subs : Model -> Sub Msg
subs model =
    Sub.batch [Material.subscriptions Mdl model, Sub.map PageMsg (Page.subs model.page)]


main : Routing.RouteUrlProgram Never Model Msg
main =
    Routing.program
        { delta2url = delta2url
        , location2messages = location2messages
        , init =
            ( model
--                | mdl =
--                    Layout.setTabsWidth 250 model.mdl
                    {- elm gives us no way to measure the actual width of tabs. We
                       hardwire it. If you add a tab, remember to update this. Find the
                       new value using:
                       document.getElementsByClassName("mdl-layout__tab-bar")[0].scrollWidth
                    -}
--              }
            , init
            )
        , view = view
        , subscriptions =
            \model -> subs model
        , update = update
        }
