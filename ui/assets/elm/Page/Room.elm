module Page.Room exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
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
import List exposing (filter, head)

import Views.DimmerC exposing (dimmerCard)
import Views.SunblindC exposing (sunblindCard)

import Data.Dimmer as DimmerM exposing (Dimmer, DimmerJson)
import Data.Sunblind as SunblindM exposing (Sunblind)
import Data.Light as LightM exposing (Light)
import Util exposing (replaceListElem)
import Data.Id as Id
import Request.Room
import Http


-- MODEL
type alias Model =
    { dimmers : List Dimmer
    , sunblinds: List Sunblind
    , blindUndrawn : Bool
    , raised : Int
    }
model : Model
model =
    { dimmers = []
    , sunblinds = []
    , blindUndrawn = False
    , raised = -1
    }

-- SUB
subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none

-- UPDATE

type Msg =
    Raise Int
    | InitSunblinds (Result Http.Error (List Sunblind))
    | InitDimmers (Result Http.Error (List DimmerJson))
    | DimToggleResponse (Result Http.Error String)
    | DimSlide Dimmer Float
    | DimToggle Dimmer
    | ToggleLight Dimmer Light
    | UndrawDimmer Dimmer
    | UndrawSunblinds
    | SetAllSunblinds
    | ToggleSunblind Sunblind



update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        DimToggleResponse _->
            (model, Cmd.none)
        InitSunblinds (Ok sunblinds) ->
            ({model | sunblinds = sunblinds}, Request.Room.loadDimmers)
        InitSunblinds (Err _) ->
            (model, Cmd.none)
        InitDimmers (Ok dimmers) ->
            let
--                newDimmers = List.map2 DimmerM.updateExt dimmers model.dimmers
                newDimmers = List.map DimmerM.fromJson dimmers
            in
            ({model | dimmers = newDimmers}, Cmd.none)
        InitDimmers (Err error) ->
            let
                _=Debug.log "error" (toString error)
            in
            (model, Cmd.none)
        Raise k ->
            ({ model | raised = k}, Cmd.none)
        DimSlide dim val ->
            ({model | dimmers = replaceListElem model.dimmers (DimmerM.setFill dim val)}, Request.Room.setDimFill dim.id (round val))
        DimToggle dim ->
            ({model | dimmers = replaceListElem model.dimmers (DimmerM.toggle dim)}, Request.Room.toggleDimmer dim.id)
        ToggleLight dim light ->
            let
                newLights = replaceListElem dim.lights (LightM.toggle light)
                newDim = {dim | lights = newLights}
                isOn = DimmerM.isOn newDim
                newFill =
                    if dim.fill == 0 && isOn then 100
                    else if isOn then dim.fill
                    else 0
                newDim2 = {newDim | fill = newFill}
                newModel = {model | dimmers = replaceListElem model.dimmers newDim2}
            in
            (newModel, Request.Room.toggleDimLight light.id)
        UndrawDimmer dimmer ->
            ({model | dimmers = replaceListElem model.dimmers (DimmerM.toggleUndrawn dimmer)}, Cmd.none)
        UndrawSunblinds ->
            ({model | blindUndrawn = not model.blindUndrawn}, Cmd.none)
        SetAllSunblinds->
            ({model | sunblinds = SunblindM.toggleList model.sunblinds}, Cmd.none)
        ToggleSunblind sunblind ->
            ({model | sunblinds = replaceListElem model.sunblinds (SunblindM.toggle sunblind)}, Request.Room.toggleSunblind sunblind.id)

-- VIEW

view : Model -> Html Msg
view model =
    Options.div
    [ css "margin" "auto"
    , css "padding-left" "8%"
    , css "padding-right" "8%"
    ]
    [ Options.div
            [ css "display" "flex"
            , css "flex-flow" "row wrap"
            , css "align-items" "stretch"
            , css "width" "95%"
            , css "margin" "1rem"
            ]
            ((makeSunblind model) :: (makeDimList model))
        ]


makeSunblind : Model -> Html Msg
makeSunblind model =
    sunblindCard model.mdl model.sunblinds -2 model.raised model.blindUndrawn

makeDimList : Model -> List (Html Msg)
makeDimList model =
    List.map (\x -> dimmerCard model.mdl x (DimmerM.getId x) model.raised) model.dimmers


