module Main exposing (main)

import Bootstrap.Button as Button
import Bootstrap.CDN as CDN
import Bootstrap.Card as Card
import Bootstrap.Card.Block as Block
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.ListGroup as Listgroup
import Bootstrap.Modal as Modal
import Browser exposing (UrlRequest)
import Browser.Navigation as Navigation
import Dashboard exposing (DashboardShort)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Http
import Page exposing (Page)
import Page.Dashboard as Dashboard
import Page.Management as Management
import Page.Navbar as Navbar
import Route
import Url exposing (Url)
import Url.Parser as UrlParser exposing ((</>), Parser, s, top)


type alias Flags =
    {}


type Content
    = DashboardContent Dashboard.Model
    | ManagementContent Management.Model
    | NotFound


type alias Model =
    { navKey : Navigation.Key
    , navModel : Navbar.Model Msg
    , content : Content
    }


main : Program Flags Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlRequest = ClickedLink
        , onUrlChange = UrlChange
        }


init : Flags -> Url -> Navigation.Key -> ( Model, Cmd Msg )
init flags url key =
    let
        ( navModel, navCmd ) =
            Navbar.init NavMsg LoadDashboardTabs

        ( model, urlCmd ) =
            urlUpdate url { navKey = key, navModel = navModel, content = NotFound }
    in
    ( model, Cmd.batch [ urlCmd, navCmd ] )


type Msg
    = UrlChange Url
    | ClickedLink UrlRequest
    | NavMsg Navbar.State
    | LoadDashboardTabs (Result Http.Error (List DashboardShort))
    | DashboardMsg Dashboard.Msg
    | ManagementMsg Management.Msg


subscriptions : Model -> Sub Msg
subscriptions model =
    Navbar.subscriptions model.navModel


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        nop =
            ( model, Cmd.none )

        noCmd : Model -> ( Model, Cmd Msg )
        noCmd model_ =
            ( model_, Cmd.none )
    in
    case ( msg, model.content ) of
        ( ClickedLink req, _ ) ->
            case req of
                Browser.Internal url ->
                    case url.fragment of
                        Just "" ->
                            ( model, Cmd.none )

                        _ ->
                            ( model, Navigation.pushUrl model.navKey <| Url.toString url )

                Browser.External href ->
                    ( model, Navigation.load href )

        ( UrlChange url, _ ) ->
            urlUpdate url model

        ( NavMsg state, _ ) ->
            let
                update_ { navModel } =
                    { navModel | state = state }
            in
            ( { model | navModel = update_ model }
            , Cmd.none
            )

        ( LoadDashboardTabs (Ok list_), _ ) ->
            let
                tabs =
                    List.map (\{ name, id } -> ( name, Route.Dashboard id )) list_

                update_ { navModel } =
                    { navModel | dashboardTabs = Just tabs }
            in
            noCmd { model | navModel = update_ model }

        ( LoadDashboardTabs (Err _), _ ) ->
            nop

        ( DashboardMsg msg_, DashboardContent model_ ) ->
            Dashboard.update msg_ model_
                |> updateWith DashboardContent DashboardMsg model

        ( ManagementMsg msg_, ManagementContent model_ ) ->
            Management.update msg_ model_
                |> updateWith ManagementContent ManagementMsg model

        ( _, _ ) ->
            ( model, Cmd.none )


updateWith : (subModel -> Content) -> (subMsg -> Msg) -> Model -> ( subModel, Cmd subMsg ) -> ( Model, Cmd Msg )
updateWith toModel toMsg model ( subModel, subCmd ) =
    ( { model | content = toModel subModel }
    , Cmd.map toMsg subCmd
    )


urlUpdate : Url -> Model -> ( Model, Cmd Msg )
urlUpdate url model =
    case Route.fromUrl url of
        Nothing ->
            ( model, Cmd.none )

        Just route ->
            let
                _ =
                    Debug.log "" (Debug.toString route)
            in
            case route of
                Route.Management s ->
                    ( { model | content = ManagementContent Management.mdl }, Cmd.none )

                Route.Dashboard id ->
                    ( { model | content = DashboardContent Dashboard.mdl }, Cmd.map DashboardMsg <| Dashboard.init id )

                Route.Home ->
                    -- TODO repair this
                    ( { model | content = NotFound }, Cmd.none )


view : Model -> Browser.Document Msg
view model =
    let
        config : Page.Config Msg
        config =
            Page.config model.navModel
    in
    case model.content of
        DashboardContent model_ ->
            config
                |> Page.withPage Page.Dashboard "Dashboard"
                |> Page.withContent DashboardMsg Dashboard.view model_
                |> Page.view

        ManagementContent model_ ->
            config
                |> Page.withPage Page.Management "Management"
                |> Page.withContent ManagementMsg Management.view model_
                |> Page.view

        NotFound ->
            config
                |> Page.view
