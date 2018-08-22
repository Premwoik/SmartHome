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

import Data.Dimmer as DimmerM exposing (Dimmer)
import Data.Sunblind as SunblindM exposing (Sunblind)
import Data.Light as LightM exposing (Light)
import Util exposing (replaceListElem)
import Model exposing (..)
import Data.Id as Id
import Request.Room

main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


init : ( Model, Cmd Msg )
init =
    ( Model [] [] initBlindUndrawn -1 Material.model
    , Request.Room.loadSunblinds
    )

initBlindUndrawn : Bool
initBlindUndrawn =
    False

--initSunblinds : List Sunblind
--initSunblinds =
--    [ Sunblind (Id.Id 0) "T1" SunblindM.Open
--    , Sunblind (Id.Id 1) "T2" SunblindM.Open
--    , Sunblind (Id.Id 2) "T3" SunblindM.Open
--    , Sunblind (Id.Id 3) "T4" SunblindM.Open
--    , Sunblind (Id.Id 4) "T5" SunblindM.Open
--    ]
--
--initDimmers : List Dimmer
--initDimmers =
--    [ Dimmer (Id.Id 0) "Tv" DimmerM.Up 0 [Light (Id.Id 0) LightM.On "Test", Light (Id.Id 1) LightM.On "Test2", Light (Id.Id 2) LightM.On "Test3"] False
--    , Dimmer (Id.Id 1) "Sofa" DimmerM.Up 0 [] False
--    , Dimmer (Id.Id 2) "Kuchnia" DimmerM.Up 0 [] False
--    ]


-- SUB

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none

-- UPDATE





update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
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
            ({model | dimmers = replaceListElem model.dimmers (DimmerM.setFill dim val)}, Cmd.none)
        DimToggle dim ->
            ({model | dimmers = replaceListElem model.dimmers (DimmerM.toggle dim)}, Cmd.none)
        ToggleLight dim light ->
            let
                newDim = {dim | lights = replaceListElem dim.lights (LightM.toggle light)}
                newModel = {model | dimmers = replaceListElem model.dimmers newDim}
            in
            (newModel, Cmd.none)
        UndrawDimmer dimmer ->
            ({model | dimmers = replaceListElem model.dimmers (DimmerM.toggleUndrawn dimmer)}, Cmd.none)
        UndrawSunblinds ->
            ({model | blindUndrawn = not model.blindUndrawn}, Cmd.none)
        SetAllSunblinds->
            ({model | sunblinds = SunblindM.toggleList model.sunblinds}, Cmd.none)
        ToggleSunblind sunblind ->
            ({model | sunblinds = replaceListElem model.sunblinds (SunblindM.toggle sunblind)}, Cmd.none)
        Mdl action_ ->
            Material.update Mdl action_ model
--        default ->
--            Model.update default model

-- VIEW

view : Model -> Html Msg
view model =
    Scheme.topWithScheme Color.Cyan Color.Lime <|
        Layout.render Mdl
            model.mdl
            [ Layout.fixedHeader ]
            { header = [ header ]
            , drawer = []
            , tabs = ( [], [] )
            , main = [ viewContent model ]
            }

header : Html Msg
header =
    div []
        [ h5
            [ style
                [ ( "float", "left" )
                , ( "padding-left", "40px" )
                ]
            ]
            [ text "easyHome" ]
        ]


viewContent : Model -> Html Msg
viewContent model =
    Options.div
        [ css "display" "flex"
        , css "flex-flow" "row wrap"
        , css "align-items" "flex-start"
        , css "width" "95%"
        , css "margin" "1rem"
        ]
        ((makeSunblind model) :: (makeDimList model))


makeSunblind : Model -> Html Msg
makeSunblind model =
    sunblindCard model.mdl model.sunblinds -2 model.raised model.blindUndrawn

makeDimList : Model -> List (Html Msg)
makeDimList model =
    List.map (\x -> dimmerCard model.mdl x (DimmerM.getId x) model.raised) model.dimmers


