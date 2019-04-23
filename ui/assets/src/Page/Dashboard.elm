module Page.Dashboard exposing (Model, Msg, init, mdl, update, view)

import Action exposing (Action)
import Bootstrap.Card as Card
import Bootstrap.Grid as Grid exposing (Column)
import Bootstrap.Grid.Col as Col
import Bootstrap.Grid.Row as Row
import Dashboard exposing (Dashboard, updateContent, withAction, withDimmer, withLight, withPort, withSunblind, withTask)
import Dimmer exposing (Dimmer)
import Html exposing (..)
import Html.Attributes exposing (href, style)
import Http
import Light exposing (Light)
import Page.Dashboard.ActionCard as ActionCard
import Page.Dashboard.DimmerCard as DimmerCard
import Page.Dashboard.ExampleCard as ExampleCard
import Page.Dashboard.LightCard as LightCard
import Page.Dashboard.PortCard as PortCard
import Page.Dashboard.SunblindCard as SunblindCard
import Page.Dashboard.TaskCard as TaskCard
import Port exposing (Port)
import Sunblind exposing (Sunblind)
import TaskM exposing (Task)


type alias Model =
    { dash : Maybe Dashboard
    }


mdl : Model
mdl =
    { dash = Nothing }


init : Int -> Cmd Msg
init id =
    Dashboard.getDashboard id LoadedDashboard


type Msg
    = Nop
    | LoadedDashboard (Result Http.Error Dashboard)
    | TogglePort Port
    | ToggleLight Light
    | UpdateDimmer Dimmer
    | ToggleAction Action
    | ToggleSunblind Sunblind
    | ToggleSunblindMode Sunblind
    | ToggleTask Task
    | ResponsePort (Result Http.Error Port)
    | ResponseLight (Result Http.Error Light)
    | ResponseDimmer (Result Http.Error Dimmer)
    | ResponseSunblind (Result Http.Error Sunblind)
    | ResponseAction (Result Http.Error Action)
    | ResponseTask (Result Http.Error Task)



--| ResponseTask (Result Http.Error Task)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        nop =
            ( model, Cmd.none )

        noCmd : Model -> ( Model, Cmd Msg )
        noCmd model_ =
            ( model_, Cmd.none )

        cmd : Cmd Msg -> Model -> ( Model, Cmd Msg )
        cmd cmd_ model_ =
            ( model_, cmd_ )

        wrapDash : Dashboard -> Model
        wrapDash d =
            { model | dash = Just d }

        wrapContent : Dashboard -> List Dashboard.Content -> Dashboard
        wrapContent d c =
            { d | content = c }

        wrap : Dashboard -> List Dashboard.Content -> Model
        wrap d c =
            wrapDash <| wrapContent d c

        _ =
            Debug.log "MSG" (Debug.toString msg)
    in
    case ( msg, model.dash ) of
        ( LoadedDashboard (Ok dash), _ ) ->
            noCmd { model | dash = Just dash }

        ( LoadedDashboard (Err _), _ ) ->
            noCmd { model | dash = Nothing }

        ( TogglePort p, Just d ) ->
            cmd (Port.toggle Nothing ResponsePort p) << wrap d <| d.content

        ( ToggleLight l, Just d ) ->
            cmd (Light.toggle Nothing ResponseLight l) << wrap d <| d.content

        ( ToggleAction a, Just d ) ->
            cmd (Action.toggle Nothing ResponseAction a) << wrap d <| d.content

        ( ToggleSunblind s, Just d ) ->
            cmd (Sunblind.click Nothing ResponseSunblind s) << wrap d <| d.content

        ( ToggleSunblindMode s, Just d ) ->
            cmd (Sunblind.toggleManual Nothing ResponseSunblind s) << wrap d <| d.content

        ( UpdateDimmer dim, Just d ) ->
            cmd (Dimmer.toggle Nothing ResponseDimmer dim) << wrap d <| d.content

        ( ToggleTask t, Just d ) ->
            cmd (TaskM.toggle Nothing ResponseTask t) << wrap d <| d.content

        ( ResponsePort (Ok p), Just d ) ->
            noCmd << wrap d << updateContent p withPort <| d.content

        ( ResponsePort (Err _), Just d ) ->
            nop

        ( ResponseLight (Ok l), Just d ) ->
            noCmd << wrap d << updateContent l withLight <| d.content

        ( ResponseLight (Err _), Just d ) ->
            nop

        ( ResponseAction (Ok a), Just d ) ->
            noCmd << wrap d << updateContent a withAction <| d.content

        ( ResponseAction (Err _), Just d ) ->
            nop

        ( ResponseTask (Ok t), Just d ) ->
            noCmd << wrap d << updateContent t withTask <| d.content

        ( ResponseTask (Err _), Just d ) ->
            nop

        ( ResponseSunblind (Ok s), Just d ) ->
            noCmd << wrap d << updateContent s withSunblind <| d.content

        ( ResponseSunblind (Err _), Just d ) ->
            nop

        ( ResponseDimmer (Ok dim), Just d ) ->
            noCmd << wrap d << updateContent dim withDimmer <| d.content

        ( ResponseDimmer (Err _), Just d ) ->
            nop

        ( _, _ ) ->
            nop


view : Model -> Html Msg
view model =
    case model.dash of
        Just dash ->
            viewContent dash

        Nothing ->
            viewError


genColumns : Dashboard -> List (Card.Config Msg)
genColumns { content } =
    let
        _ =
            Debug.log "LIST" (Debug.toString content)
    in
    List.map
        (\element ->
            case element of
                Dashboard.Port p ->
                    PortCard.card TogglePort p

                Dashboard.Dimmer d ->
                    DimmerCard.card UpdateDimmer d

                Dashboard.Light l ->
                    LightCard.card ToggleLight l

                Dashboard.Action a ->
                    ActionCard.card ToggleAction a

                Dashboard.Sunblind s ->
                    SunblindCard.card ToggleSunblind ToggleSunblindMode s

                Dashboard.Task t ->
                    TaskCard.card ToggleTask t
         --_ ->
         --ExampleCard.card
        )
        content


viewContent : Dashboard -> Html Msg
viewContent d =
    Card.columns <| genColumns d


viewError : Html Msg
viewError =
    div [] []
