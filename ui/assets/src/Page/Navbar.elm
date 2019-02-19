module Page.Navbar exposing (Model, State, init, subscriptions, view)

import Bootstrap.Navbar as Navbar exposing (State)
import Dashboard exposing (DashboardShort)
import Html exposing (..)
import Html.Attributes exposing (style)
import Http
import Route exposing (Route, href)


type alias State =
    Navbar.State


type alias NavDropdownData =
    Maybe (List ( String, Route ))


type alias Model msg =
    { state : State
    , msg : State -> msg
    , managementTabs : NavDropdownData
    , dashboardTabs : NavDropdownData
    }


managementTabs : NavDropdownData
managementTabs =
    Just
        [ ( "Urządzenia", Route.Management "devices" )
        , ( "Rolety", Route.Management "sunblinds" )
        , ( "Ściemniacze", Route.Management "dimmers" )
        , ( "Światła", Route.Management "lights" )
        , ( "Akcje", Route.Management "actions" )
        ]


init : (State -> msg) -> (Result Http.Error (List DashboardShort) -> msg) -> ( Model msg, Cmd msg )
init navMsg loadDashboardTabsMsg =
    let
        ( navState, navCmd ) =
            Navbar.initialState navMsg

        model_ =
            Model navState navMsg managementTabs Nothing

        cmd_ =
            Cmd.batch [ navCmd, Dashboard.getTabs loadDashboardTabsMsg ]
    in
    ( model_, cmd_ )


subscriptions : Model msg -> Sub msg
subscriptions { state, msg } =
    Navbar.subscriptions state msg


view : Model msg -> Html msg
view model_ =
    let
        navbarItems =
            []

        items =
            navbarItems
                |> dropdown model_.managementTabs "Management" "nav_management_dropdown"
                |> dropdown model_.dashboardTabs "Dashboard" "nav_dashboard_drowdown"
                |> item "Home" Route.Home
    in
    Navbar.config model_.msg
        |> Navbar.withAnimation
        --|> Navbar.collapseLarge
        |> Navbar.dark
        |> Navbar.brand [ style "color" "white", href Route.Home ] [ text "SmartHome" ]
        |> Navbar.items items
        |> Navbar.view model_.state


item : String -> Route -> List (Navbar.Item msg) -> List (Navbar.Item msg)
item name route items =
    let
        item_ =
            Navbar.itemLink [ href route ] [ text name ]
    in
    item_ :: items


dropdown : NavDropdownData -> String -> String -> List (Navbar.Item msg) -> List (Navbar.Item msg)
dropdown data name id items =
    let
        item_ list_ =
            Navbar.dropdown
                { id = id
                , toggle = Navbar.dropdownToggle [] [ text name ]
                , items =
                    List.map
                        (\( name_, route ) ->
                            Navbar.dropdownItem [ href route ] [ text name_ ]
                        )
                        list_
                }
    in
    case data of
        Nothing ->
            items

        Just list_ ->
            item_ list_ :: items
