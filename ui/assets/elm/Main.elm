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
import Page.Room
import Page.Room.Model
import Material.Helpers exposing (lift)

-- MODEL

type alias Model =
    { mdl : Material.Model
    , rooms : Page.Room.Model.Model
    , selectedTab : Int
    }

model : Model
model =
    { mdl = Material.model
    , selectedTab = 0
    , rooms = Page.Room.Model.model
    }

-- UPDATE

type Msg
    = SelectTab Int
    | Mdl (Material.Msg Msg)
    | RoomMsg Page.Room.Model.Msg



update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SelectTab k ->
            ( { model | selectedTab = k }, Array.get k tabInit |> Maybe.withDefault Cmd.none)
        Mdl action_ ->
            Material.update Mdl action_ model
        RoomMsg a ->
            lift .rooms (\m x -> { m | rooms = x }) RoomMsg Page.Room.update a model


-- VIEW

view : Model -> Html Msg
view =
    Html.Lazy.lazy view_


view_ : Model -> Html Msg
view_ model =
    let
        top =
            (Array.get model.selectedTab tabViews |> Maybe.withDefault e404) model
    in
      Scheme.top <|  Layout.render Mdl
            model.mdl
            [ Layout.selectedTab model.selectedTab
            , Layout.onSelectTab SelectTab
            , Layout.scrolling
            ]
            { drawer = []
            , header = header
            , main = [top]
            , tabs =
                    ( tabTitles, [] )
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


tabs : List ( String, String, Model -> Html Msg, Cmd Msg )
tabs =
    [ ( "Rooms", "rooms", .rooms >> Page.Room.view >> Html.map RoomMsg, Page.Room.init |> Cmd.map RoomMsg)
    ]

tabInit : Array (Cmd Msg)
tabInit =
    List.map (\(_, _, _, x) -> x) tabs |> Array.fromList

tabTitles : List (Html a)
tabTitles =
    List.map (\( x, _, _, _ ) -> text x) tabs


tabViews : Array (Model -> Html Msg)
tabViews =
    List.map (\( _, _, v, _ ) -> v) tabs |> Array.fromList


tabUrls : Array String
tabUrls =
    List.map (\( _, x, _, _) -> x) tabs |> Array.fromList


urlTabs : Dict String Int
urlTabs =
    List.indexedMap (\idx ( _, x, _, _ ) -> ( x, idx )) tabs |> Dict.fromList


e404 : Model -> Html Msg
e404 _ =
    div
        []
        [ Options.styled Html.h1
            [ Options.cs "mdl-typography--display-4"
            , Typography.center
            ]
            [ text "404" ]
        ]


-- ROUTING




urlOf : Model -> String
urlOf model =
    "#" ++ (Array.get model.selectedTab tabUrls |> Maybe.withDefault "")

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

        x ->
            Dict.get x urlTabs
                |> Maybe.withDefault -1
                |> SelectTab
    ]

main : Routing.RouteUrlProgram Never Model Msg
main =
    Routing.program
        { delta2url = delta2url
        , location2messages = location2messages
        , init =
            ( { model
                | mdl =
                    Layout.setTabsWidth 250 model.mdl
                    {- elm gives us no way to measure the actual width of tabs. We
                       hardwire it. If you add a tab, remember to update this. Find the
                       new value using:
                       document.getElementsByClassName("mdl-layout__tab-bar")[0].scrollWidth
                    -}
              }
            , Material.init Mdl
            )
        , view = view
        , subscriptions =
            \model ->
                Sub.batch
                    [ Sub.none
                    , Material.subscriptions Mdl model
                    ]
        , update = update
        }
