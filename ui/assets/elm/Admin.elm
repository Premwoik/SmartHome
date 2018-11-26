module Admin exposing (..)

import Array exposing (Array)

import Html exposing (div, text, Html, p)


import Material.Table as Table exposing (table, th, thead, tr, td, tbody, ascending, numeric)
import Material.Options as Options exposing (css)
import Material.Button as Button
import Material.Dialog as Dialog
import Material.Grid as Grid
import Material.Textfield as Textfield
import Material.Toggles as Toggles
import Material

import Admin.Action as Action exposing (Action)
import Admin.Device as Device exposing (Device)
import Admin.Dimmer as Dimmer exposing (Dimmer)
import Admin.Port as Port exposing (Port)
import Admin.Dashboard as Dashboard exposing (Dashboard)

import Dom.Scroll as DS
import Dom

import Page
import Task
import Http
import Request

import Select

type alias Item = ( Int, String, String, String )

type TabType
    = TabActions
    | TabDevices
    | TabPorts
    | TabDimmers
    | TabDashboards

type EditData
    = EditActionData Action
    | EditDeviceData Device
    | EditDimmerData Dimmer
    | EditPortData Port
    | EditDashboardData Dashboard
    | None

type Edit
    = EditAction EditActionField
    | EditDimmer EditDimmerField
    | EditDevice EditDeviceField
    | EditDashboard EditDashboardField
    | EditPort EditPortField

type EditActionField
    = EditActionName String
    | EditActionFunction String
    | EditActionActive Bool
    | EditActionParams String
    | EditActionFrequency (Result String Int)
    | EditActionStartTime String
    | EditActionEndTime String
--    | EditActionPort Int

type EditPortField
    = EditPortName String
    | EditPortMode String
    | EditPortState Bool
    | EditPortType String
    | EditPortTimeout (Result String Int)
--    | EditPortDeviceId Int

type EditDeviceField
    = EditDeviceName String
    | EditDeviceIp String
    | EditDevicePort (Result String Int)
    | EditDeviceType String

type EditDimmerField
    = EditDimmerName String
    | EditDimmerType String
    | EditDimmerTime (Result String Int)
    | EditDimmerFullTime (Result String Int)
    | EditDimmerFill (Result String Int)

type EditDashboardField
    = EditDashboardName String
    | EditDashboardTitle String
    | EditDashboardDesc String
    | EditDashboardOrder (Result String Int)
    | EditDashboardDimmers String
    | EditDashboardLights String
    | EditDashboardDevices String
    | EditDashboardActions String
    | EditDashboardTasks String
    | EditDashboardPorts String
    | EditDashboardSunblinds String

type alias Model =
    { selectedTab : Int
    , mdl : Material.Model
    , tab : TabType
    , creator : Maybe Int
    , devices : List Device
    , ports : List Port
    , dimmers : List Dimmer
    , actions : List Action
    , dashboards : List Dashboard
    , editIndex : Int
    , editData : EditData
    }


model : Model
model =
    { selectedTab = -1
    , mdl = Material.model
    , tab = TabActions
    , creator = Nothing
    , devices = []
    , ports = []
    , dimmers = []
    , actions = []
    , dashboards = []
    , editIndex = -1 -- index -1 means that item is not selected and new will be create, otherwise modify index item
    , editData = None
    }




init : Model -> Cmd Msg
init model =
    if model.selectedTab == -1 then Task.succeed (SelectTab 1) |> Task.perform identity else Cmd.none

subs : Model -> Sub Msg
subs _ =
    Sub.none

tabs : List (String, String, Cmd Msg)
tabs =
    [ ("Panele", "dashboards", Request.send LoadDashboards Dashboard.getDashboards)
    , ("Urządzenia", "devices", Request.send LoadDevices Device.getDevices)
    , ("Ściemniacze", "dimmers", Request.send LoadDimmers Dimmer.getDimmers)
    , ("Akcje", "actions", Request.send LoadActions Action.getActions)
    , ("Inne", "others", Request.send LoadPorts Port.getPorts)
    , ("_____", "____", Cmd.none)
    ]

tabsTitles : List String
tabsTitles =
    tabs |> List.map (\(x, _, _) -> x)

tabsUrls : Array String
tabsUrls =
    tabs |> List.map (\(_, x, _) -> x) >> Array.fromList

tabsInit : Array (Cmd Msg)
tabsInit =
    tabs |> List.map (\(_, _, x) -> x) >> Array.fromList


type Msg
    = SelectTab Int

    | EditIndex Int
    | EditMsg Edit
    | EditSubmit
    | EditCancel
    | Create

    | DeleteMark Int
    | DeleteUnmark
    | Delete
    | DeleteResp (Result Http.Error ())

    | LoadDashboards (Result Http.Error (List Dashboard))
    | LoadActions (Result Http.Error (List Action))
    | LoadDevices (Result Http.Error (List Device))
    | LoadDimmers (Result Http.Error (List Dimmer))
    | LoadPorts (Result Http.Error (List Port))

    | UpdateDashboard (Result Http.Error Dashboard)
    | UpdateAction (Result Http.Error Action)
    | UpdateDevice (Result Http.Error Device)
    | UpdateDimmer (Result Http.Error Dimmer)
    | UpdatePort (Result Http.Error Port)

    | Mdl (Material.Msg Msg)
    | Nop


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    let
        skip = (model, Cmd.none)
        cmd c = (model, c)
        packData x = ({model | editData = x}, Cmd.none)
        update d l =
            case model.editIndex of
                -1 ->
                    d :: l
                _ ->
                    List.map (\x -> if x.id == d.id then d else x) l
        _ = Debug.log "Msg: " msg
    in
    case msg of
        SelectTab k ->
            ( {model | selectedTab = k, editIndex = -1, editData = None}, tabsInit |> Array.get k >> Maybe.withDefault Cmd.none)

        EditIndex k ->
            let
                getNMap x y =
                   model |> x >> Array.fromList >> Array.get k >> Maybe.map y >> Maybe.withDefault None
                data =
                    case model.tab of
                        TabActions ->
                            getNMap .actions EditActionData
                        TabDevices ->
                            getNMap .devices EditDeviceData
                        TabPorts ->
                            getNMap .ports EditPortData
                        TabDimmers ->
                            getNMap .dimmers EditDimmerData
                        TabDashboards ->
                            getNMap .dashboards EditDashboardData
            in
            ( {model | editIndex = k, editData = data}, Cmd.none)

        Create->
          let
            data =
                case model.tab of
                    TabActions ->
                        EditActionData (Action.empty)
                    TabDevices ->
                        EditDeviceData (Device.empty)
                    TabDimmers ->
                        EditDimmerData (Dimmer.empty)
                    TabPorts ->
                        EditPortData (Port.empty)
                    TabDashboards ->
                        EditDashboardData (Dashboard.empty)
          in
          ( {model | editData = data, editIndex = -1}, Cmd.none )


        DeleteMark k->
            ( {model | editIndex = k}, Cmd.none )
        DeleteUnmark ->
            ( {model | editIndex = -1}, Cmd.none )

        Delete ->
            let
                k = model.editIndex
                cmd =
                    case model.tab of
                        TabActions ->
                            Request.send DeleteResp (Action.deleteAction k)
                        TabPorts ->
                            Request.send DeleteResp (Port.deletePort k)
                        TabDimmers ->
                            Request.send DeleteResp (Dimmer.deleteDimmer k)
                        TabDevices ->
                            Request.send DeleteResp (Device.deleteDevice k)
                        TabDashboards ->
                            Request.send DeleteResp (Dashboard.deleteDashboard k)
            in
            ( {model | editIndex = k}, cmd)

        DeleteResp (Ok ()) ->
            let
                delete l = List.filter (\x -> x.id /= model.editIndex) l
                data =
                    case model.tab of
                        TabActions ->
                            {model | actions = delete model.actions}
                        TabDevices ->
                            {model | devices = delete model.devices}
                        TabDimmers ->
                            {model | dimmers = delete model.dimmers}
                        TabPorts ->
                            {model | ports = delete model.ports}
                        TabDashboards ->
                            {model | dashboards = delete model.dashboards}
            in
            ( {data | editIndex = -1}, Cmd.none )
        DeleteResp (Err _) ->
            skip


        EditMsg edit ->
            case (edit, model.editData) of
                (EditAction field, EditActionData data)->
                    let
                        pack x = packData (EditActionData x)
                    in
                    case field of
                        EditActionName n ->
                            data |> pack
                        EditActionFunction f ->
                            {data | function = f} |> pack
                        EditActionActive s ->
                            {data | active = s} |> pack
                        EditActionParams p ->
                            {data | params = p} |> pack
                        EditActionFrequency (Ok f) ->
                            {data | frequency = f} |> pack
                        EditActionFrequency (Err _) ->
                            skip
                        EditActionStartTime s ->
                            {data | startTime = Just s} |> pack
                        EditActionEndTime e ->
                            {data | endTime = Just e} |> pack
                (EditPort field, EditPortData data) ->
                    let
                        pack x = packData (EditPortData x)
                    in
                    case field of
                        EditPortName n ->
                            {data | name = n} |> pack
                        EditPortMode m ->
                            {data | mode = m} |> pack
                        EditPortType t ->
                            {data | type_ = t} |> pack
                        EditPortState s ->
                            {data | state = s } |> pack
                        EditPortTimeout (Ok t) ->
                            {data | timeout = t} |> pack
                        EditPortTimeout (Err _) ->
                            skip
                (EditDimmer field, EditDimmerData data) ->
                    let
                        pack x = packData (EditDimmerData x)
                        port1_ = data.port_
                        _ = Debug.log "Field:" field
                        _ = Debug.log "Data:" data
                    in
                    case field of
                        EditDimmerName n ->
                            {data | port_ = {port1_ | name = n}} |> pack
                        EditDimmerType t ->
                            skip --TODO
--                            {dat | type_ = t} |> pack
                        EditDimmerTime (Ok t) ->
                            {data | port_ = {port1_ | timeout = t}} |> pack
                        EditDimmerTime (Err _) ->
                            skip
                        EditDimmerFullTime (Ok t) ->
                            {data | fullTime = t} |> pack
                        EditDimmerFullTime (Err _) ->
                            skip
                        EditDimmerFill (Ok f) ->
                            {data | fill = f} |> pack
                        EditDimmerFill (Err _) ->
                            skip
                (EditDevice field, EditDeviceData data) ->
                     let
                        pack x = packData (EditDeviceData x)
                     in
                     case field of
                         EditDeviceName n ->
                             {data | name = n} |> pack
                         EditDeviceIp ip ->
                             {data | ip = ip} |> pack
                         EditDevicePort (Ok p) ->
                             {data | port_ = p} |> pack
                         EditDevicePort (Err _) ->
                             skip
                         EditDeviceType t  ->
                             {data | type_ = t} |> pack
                (EditDashboard field, EditDashboardData data) ->
                      let
                         pack x = packData (EditDashboardData x)
                      in
                      case field of
                          EditDashboardName n ->
                              {data | name = n} |> pack
                          EditDashboardTitle t ->
                              {data | title = t} |> pack
                          EditDashboardDesc d ->
                              {data | description = d} |> pack
                          EditDashboardOrder (Ok o) ->
                              {data | order = o} |> pack
                          EditDashboardOrder (Err _) ->
                              skip
                          EditDashboardSunblinds s ->
                              {data | sunblinds = s} |> pack
                          EditDashboardDimmers d ->
                              {data | dimmers = d} |> pack
                          EditDashboardLights l ->
                              {data | lights = l} |> pack
                          EditDashboardDevices d ->
                              {data | devices = d} |> pack
                          EditDashboardActions a ->
                              {data | actions = a} |> pack
                          EditDashboardTasks t ->
                              {data | tasks = t} |> pack
                          EditDashboardPorts p ->
                              {data | ports = p} |> pack
                _ ->
                    skip

        EditSubmit ->
            let
                or a b c = if model.editIndex == -1 then a c else b c
                action a = or Action.createAction Action.updateAction a
                dimmer a = or Dimmer.createDimmer Dimmer.updateDimmer a
                device a = or Device.createDevice Device.updateDevice a
                port_ a = or Port.createPort Port.updatePort a
                dashboard a = or Dashboard.createDashboard Dashboard.updateDashboard a
            in
            case model.editData of
                EditActionData a ->
                    ( model, Request.send UpdateAction (action a) )
                EditDimmerData d ->
                    ( model, Request.send UpdateDimmer (dimmer d) )
                EditDeviceData d ->
                    ( model, Request.send UpdateDevice (device d) )
                EditPortData p ->
                    ( model, Request.send UpdatePort (port_ p) )
                EditDashboardData d ->
                    ( model, Request.send UpdateDashboard (dashboard d))
                None ->
                    skip
        EditCancel ->
            ( {model | editData = None, editIndex = -1}, Cmd.none)


        LoadDashboards (Ok d) ->
            ( {model | dashboards = d, tab = TabDashboards}, Cmd.none )
        LoadDashboards (Err _) ->
            skip

        LoadActions (Ok a) ->
            ( {model | actions = a, tab = TabActions}, Cmd.none )
        LoadActions (Err _) ->
            skip

        LoadDevices (Ok d) ->
            ( {model | devices = d, tab = TabDevices}, Cmd.none )
        LoadDevices (Err _) ->
            skip

        LoadDimmers (Ok d) ->
            ( {model | dimmers = d, tab = TabDimmers}, Cmd.none )
        LoadDimmers (Err _) ->
            skip

        LoadPorts (Ok p) ->
            ( {model | ports = p, tab = TabPorts}, Cmd.none )
        LoadPorts (Err _) ->
            skip

        UpdateDashboard (Ok d) ->
            ( {model | dashboards = update d model.dashboards}
            , Cmd.none )
        UpdateDashboard (Err _) ->
            skip

        UpdateAction (Ok a) ->
            ( {model | actions = update a model.actions}
            , Cmd.none )
        UpdateAction (Err _) ->
            skip

        UpdateDevice (Ok d) ->
            ( {model | devices = update d model.devices}
            , Cmd.none )
        UpdateDevice (Err _) ->
            skip

        UpdateDimmer (Ok d) ->
            ( {model | dimmers = update d model.dimmers}
            , Cmd.none )
        UpdateDimmer (Err _) ->
            skip

        UpdatePort (Ok p) ->
            ( {model | ports = update p model.ports}
            , Cmd.none )
        UpdatePort (Err _) ->
            skip

        Mdl action_ ->
            Material.update Mdl action_ model
        Nop ->
            (model, Cmd.none)






-- VIEW

view : Model -> Html Msg
view model =
    let
        content =
            othersView model
    in
    Page.body1 "Panel zarządzania" "asdasf" (div [] []) [] content



othersView : Model -> List (Html Msg)
othersView model =
    let
        deleteDialog =
            Dialog.view
              [ ]
              [ Dialog.title [] [ text "Usuwanie" ]
              , Dialog.content []
                  [ p [] [ text "Jesteś pewien, że chcesz to usunąć?" ]
                  ]
              , Dialog.actions [ ]
                [ Button.render Mdl [3, 1] model.mdl
                    [ Dialog.closeOn "click"
                    , Options.onClick Delete]
                    [ text "Usuń" ]
                , Button.render Mdl [3, 2] model.mdl
                    [ Dialog.closeOn "click"
                    , Options.onClick DeleteUnmark]
                    [ text "Anuluj" ]
                ]

              ]
        editCell =
            if model.editData /= None then editCellView model.editData model.mdl else Grid.cell [] []
        allList =
            listedItemsView ("List", shortenItemList model.mdl (dataToItems model))
        addBtn =
            editButton model.mdl 1 "New" Create
    in
    [ addBtn
    , Grid.grid [Options.id "main-grid"]
        [ editCell
        , allList
        ]
    , deleteDialog
    ]


editCellView : EditData -> Material.Model -> Grid.Cell Msg
editCellView data mdl=
    let
        editBody =
            case data of
                 EditActionData a ->
                     editActionView a mdl
                 EditDeviceData d ->
                     editDeviceView d mdl
                 EditDimmerData d ->
                     editDimmerView d mdl
                 EditPortData p ->
                     editPortView p mdl
                 EditDashboardData d ->
                     editDashboardView d mdl
                 _ ->
                     div [] []

        view_ body =
            Options.div []
                [ Html.h4 [] [text "Edycja"]
                , body
                , Options.div []
                    [ editButton mdl 2 "Zatwierdź" EditSubmit
                    , editButton mdl 1 "Zapomnij" EditCancel
                    ]
                ]
        cell h=
            Grid.cell
                [Grid.size Grid.Desktop 8, Grid.offset Grid.Desktop 2, Grid.size Grid.Tablet 2]
                [h]

    in
    editBody |> view_ >> cell


listedItemsView : ( String, Html Msg ) -> Grid.Cell Msg
listedItemsView (header, html) =
        Grid.cell
          [Grid.size Grid.Phone 4, Grid.size Grid.Tablet 6, Grid.offset Grid.Tablet 1, Grid.size Grid.Desktop 8, Grid.offset Grid.Desktop 2]
          [ Html.h4 [] [ text header ]
          , Options.div
              [ Options.center ]
              [ html ]
          ]

dataToItems : Model -> List Item
dataToItems model =
    case model.tab of
        TabActions ->
            List.map (\x -> (x.id, x.function, "-", toString x.active)) model.actions
        TabDevices ->
            List.map (\x -> (x.id, x.name, x.type_, "-")) model.devices
        TabDimmers ->
            List.map (\x -> (x.id, x.port_.name, "-", toString x.port_.state)) model.dimmers
        TabPorts ->
            List.map (\x -> (x.id, x.name, "type: " ++ x.type_ ++ ";; mode: " ++ x.mode, toString x.state)) model.ports
        TabDashboards ->
            List.map (\x -> (x.id, x.name, "", "-")) model.dashboards

shortenItemList : Material.Model -> List Item -> Html Msg
shortenItemList mdl items =
    let
        textAlign = css "text-align" "left"
        tdCss =
            Options.many
                [ css "white-space" "nowrap"
                , css "overflow" "hidden"
                , textAlign
                , css "text-overflow" "ellipsis"
                , css "max-width" "300px"
                ]
    in
    Options.div
        [css "overflow" "auto"]
        [
        table []
            [ thead []
                [ tr []
                    [ th [numeric] [text "Id"]
                    , th [textAlign] [text "Nazwa"]
                    , th [textAlign] [text "Status"]
                    , th [textAlign] [text "Więcej"]
                    , th [textAlign] [text "Edycja"]
                    ]
                ]
            , tbody []
                (List.indexedMap (\i (id, name, desc, status) ->
                    tr []
                        [ td [] [text (toString id)]
                        , td [css "max-width" "100px"] [text name]
                        , td [] [text status]
                        , td [tdCss] [text desc]
                        , td []
                            [ Button.render Mdl [0] mdl
                                [Options.onClick (EditIndex i)]
                                [ text "Edytuj" ]
                            , Button.render Mdl [1] mdl
                                [ Dialog.openOn "click"
                                , Options.onClick (DeleteMark id)
                                ]
                                [ text "Usuń" ]
                            ]
                        ]
                ) items)
            ]
        ]

-- Edit Views

editPortView : Port -> Material.Model -> Html Msg
editPortView p mdl =
    let
       msg x = EditMsg (EditPort x)
       default t = \x -> msg (t x)
       int t = \x -> msg (t (String.toInt x) )
    in
    Options.div []
        [ editTextfield mdl 5 "Nazwa" p.name (default EditPortName)
        , editTextfield mdl 4 "Tryb" p.mode (default EditPortMode)
        , editSwitch mdl 1 "State" p.state (default EditPortState)
        , editTextfield mdl 3 "Typ" p.type_ (default EditPortType)
        , editTextfield mdl 2 "Czas" (toString p.timeout) (int EditPortTimeout)
        , editTextfield mdl 1 "Urządzenie" "" editTfNop
        ]

editDimmerView : Dimmer -> Material.Model -> Html Msg
editDimmerView d mdl=
     let
        msg x = EditMsg (EditDimmer x)
        default t = \x -> msg (t x)
        int t = \x -> msg (t (String.toInt x) )
    in
    Options.div []
        [ editTextfield mdl 5 "Nazwa" d.port_.name (default EditDimmerName)
        , editTextfield mdl 4 "Typ (solo, dependent, group" "" (default EditDimmerType)
        , editTextfield mdl 3 "Czas włączania (ms)" (toString d.port_.timeout) (int EditDimmerTime)
        , editTextfield mdl 2 "Czas pełnego rozjaśnienia (ms)" (toString d.fullTime) (int EditDimmerFullTime)
        , editTextfield mdl 1 "Aktualna jasność (0-100)" (toString d.fill) (int EditDimmerFill)
        ]


editDeviceView : Device -> Material.Model -> Html Msg
editDeviceView d mdl =
   let
        msg x = EditMsg (EditDevice x)
        default t = \x -> msg (t x)
        int t = \x -> msg (t (String.toInt x) )
    in
    Options.div []
        [ editTextfield mdl 4 "Nazwa" d.name (default EditDeviceName)
        , editTextfield mdl 3 "ip" d.ip (default EditDeviceIp)
        , editTextfield mdl 2 "port" (toString d.port_) (int EditDevicePort)
        , editTextfield mdl 1 "Type" d.type_ (default EditDeviceType)
        ]
editActionView : Action -> Material.Model -> Html Msg
editActionView a mdl =
    let
        msg x = EditMsg (EditAction x)
        default t = \x -> msg (t x)
        int t = \x -> msg (t (String.toInt x) )
    in
    Options.div []
        [ editTextfield mdl 6 "Nazwa" "" (default EditActionName)
        , editTextfield mdl 5 "Funkcja" a.function (default EditActionFunction)
        , editTextfield mdl 4 "Parametry" a.params (default EditActionParams)
        , editTextfield mdl 3 "Częstotliwość wywołania (ms)" (toString a.frequency) (int EditActionFrequency)
        , editSwitch mdl 1 "Active" a.active (default EditActionActive)
        , editTextfield mdl 2 "Aktywne od (hh:mm:ss:000000)" (Maybe.withDefault "" a.startTime) (default EditActionStartTime)
        , editTextfield mdl 1 "Aktywne do (hh:mm:ss:000000)" (Maybe.withDefault "" a.endTime) (default EditActionEndTime)
        ]

editDashboardView : Dashboard -> Material.Model -> Html Msg
editDashboardView d mdl =
    let
        msg x = EditMsg (EditDashboard x)
        default t = \x -> msg (t x)
        int t = \x -> msg (t (String.toInt x) )
        content x = x
    in
    Options.div []
        [ editTextfield mdl 11 "Nazwa" d.name (default EditDashboardName)
        , editTextfield mdl 10 "Tytuł" d.title (default EditDashboardTitle)
        , editTextfield mdl 9 "Opis" d.description (default EditDashboardDesc)
        , editTextfield mdl 8 "Kolejność" (toString d.order) (int EditDashboardOrder)
        , editTextfield mdl 7 "Światła (id,order)" (content d.lights) (default EditDashboardLights)
        , editTextfield mdl 6 "Ściemniacze (id,order)" (content d.dimmers) (default EditDashboardDimmers)
        , editTextfield mdl 5 "Rolety (id,order)" (content d.sunblinds) (default EditDashboardSunblinds)
        , editTextfield mdl 4 "Urządzenia (id,order)" (content d.devices) (default EditDashboardDevices)
        , editTextfield mdl 3 "Akcje (id,order)" (content d.actions) (default EditDashboardActions)
        , editTextfield mdl 2 "Zadania (id,order)" (content d.tasks) (default EditDashboardTasks)
        , editTextfield mdl 1 "Inne (id,order)" (content d.ports) (default EditDashboardPorts)
        ]


editCellMdlId = 2
listCellMdlId = 1

editTfNop = (\_ -> Nop)


-- Edit View Parts


editItemCss : Options.Property c m
editItemCss =
    css "margin-right" "1rem"

editSwitch : Material.Model -> Int -> String -> Bool -> (Bool -> Msg) -> Html Msg
editSwitch  mdl id label value onToggle =
    Toggles.switch Mdl [editCellMdlId, id] mdl
        [ Options.onToggle (onToggle (not value))
        , Toggles.ripple
        , Toggles.value value
        , editItemCss
        ]
        [ text label ]

editTextfield : Material.Model -> Int -> String -> String -> (String -> Msg) -> Html Msg
editTextfield mdl id label value onInput =
    Textfield.render Mdl [editCellMdlId, id] mdl
        [ Textfield.label label
        , Textfield.floatingLabel
        , Textfield.text_
        , Textfield.value value
        , Options.onInput onInput
        , editItemCss
        ]
        []

editButton : Material.Model -> Int -> String -> Msg -> Html Msg
editButton mdl id txt msg =
    Button.render Mdl [editCellMdlId,id] mdl
        [ Button.primary
        , Options.onClick msg
        , editItemCss
        ]
        [text txt]
