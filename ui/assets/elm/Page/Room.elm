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
import Material.Helpers as Helpers exposing (pure, effect)
import List exposing (filter, head)

import Page.Room.Views.DimmerC exposing (dimmerCard)
import Page.Room.Views.SunblindC exposing (sunblindCard)

import Page.Room.Model exposing (..)
import Data.Dimmer as DimmerM exposing (Dimmer, DimmerJson)
import Data.Sunblind as SunblindM exposing (Sunblind)
import Data.Light as LightM exposing (Light)
import Util exposing (replaceListElem)
import Data.Id as Id
import Request.Room exposing (..)
import Request
import Page


-- INIT
init : Cmd Msg
init =
    Request.send2 InitRoom Request.Room.loadSunblinds Request.Room.loadDimmers


-- SUB
subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none

-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        skip = (model, Cmd.none)
    in
    case msg of
        Response _->
            skip

        InitRoom (Ok (sunblinds, dimmers)) ->
            let
                newDimmers = List.map DimmerM.fromJson dimmers
            in
            { model
            | sunblinds = sunblinds
            , dimmers = newDimmers
            } |> pure
        InitRoom (Err _) ->
            skip

        Raise k ->
            { model
            | raised = k
            } |> pure

        DimSlide dim val ->
            {model
            | dimmers = replaceListElem model.dimmers (DimmerM.setFill dim val)
            } |> effect (Request.send Response <| setDimFill dim.id <| round val)

        DimToggle dim ->
            { model
            | dimmers = replaceListElem model.dimmers (DimmerM.toggle dim)
            } |> effect (Request.send Response <| toggleDimmer dim.id)

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
            newModel
            |> effect (Request.send Response <| toggleDimLight light.id)

        UndrawDimmer dimmer ->
            { model
            | dimmers = replaceListElem model.dimmers (DimmerM.toggleUndrawn dimmer)
            } |> pure

        UndrawSunblinds ->
            { model
            | blindUndrawn = not model.blindUndrawn
            } |> pure

        SetAllSunblinds->
            { model
            | sunblinds = SunblindM.toggleList model.sunblinds
            } |> pure

        ToggleSunblind sunblind ->
            { model
            | sunblinds = replaceListElem model.sunblinds (SunblindM.toggle sunblind)
            } |> effect (Request.send Response <| toggleSunblind sunblind.id)

        Mdl action_ ->
            Material.update Mdl action_ model

-- VIEW

view : Model -> Html Msg
view model =
    Page.body1
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


