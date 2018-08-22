module Views.DimmerC exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Material
import Material.Tabs as Tabs
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
import Array
import Material.Icon as Icon

import Data.Light exposing (Light)
import Data.Dimmer exposing (Dimmer)
import Views.Card exposing (card)

import Model exposing (..)
import Data.Dimmer as DimmerM exposing (Dimmer)
import Data.Sunblind as SunblindM exposing (Sunblind)
import Data.Light as LightM exposing (Light)






turningBtn : Material.Model -> Dimmer -> Html Msg
turningBtn mdl dimmer=
    let
        state = if DimmerM.isOn dimmer then Button.colored else Options.nop
    in
    Button.render Mdl [0] mdl
      [ Button.fab
      , Options.css "align" "center"
      , Options.onClick (DimToggle dimmer)
      , state
      ]
      [ Icon.i "highlight"]


dimmerCard : Material.Model -> Dimmer -> Int -> Int -> Html Msg
dimmerCard mdl dimmer k raised =
    let
        action =
            Card.actions []
            [ Options.div
                     [ css "display" "flex"
                     , css "flex-direction" "column"
                     ]
                     [ Card.subhead
                             [ css "display" "flex"
            --                                 , css "justify-content" "space-between"
                             , css "align-items" "center"
                             , css "padding" ".3rem 2.5rem"
                             ]
                             [ turningBtn mdl dimmer
                             ]
                          ,  Card.subhead []
                                [ slider dimmer]
                          , Card.subhead
                             [ css "display" "flex"
                             , css "align-items" "center"
                             , css "padding" ".3rem 2.5rem"
                             ]
                             [ toggleBtn mdl dimmer
                             ]
                          , Options.div [] (renderLights mdl dimmer)

                     ]
            ]
    in
    card "ściemniacz" dimmer.name k raised action


slider : Dimmer -> Html Msg
slider dim =
    Slider.view
      [ Slider.step 25
      , Slider.value dim.fill
      , Slider.onChange (DimSlide dim)
      ]

cell =
  css "width" "64px"

color =
  Color.color Color.Amber Color.S500

icon = "wb_sunny"

renderLights : Material.Model -> Dimmer -> List (Html Msg)
renderLights mdl dimmer =
    let
        render =
            List.map (\x -> lightRow mdl dimmer x) dimmer.lights
    in
    if dimmer.undrawn then render else []

lightRow : Material.Model -> Dimmer -> Light -> Html Msg
lightRow mdl dimmer light =
     Card.subhead
        [ css "display" "flex"
        , css "justify-content" "space-between"
        , css "align-items" "center"
        , css "padding" ".3rem 2rem"
        ]
        [ Options.span [ cell ] [ text light.name ]
        , Options.span [ cell, css "text-align" "center" ]
            [ Icon.view icon [ Color.text color, Icon.size18 ] ]
        , Options.span [ cell, css "text-align" "right" ]
            [ toggleLightBtn mdl dimmer light
            ]
        ]

toggleLightBtn : Material.Model -> Dimmer -> Light -> Html Msg
toggleLightBtn mdl dimmer light =
    let
        state = if LightM.isOn light then Button.colored else Options.nop
    in
    Button.render Mdl [1, LightM.getId light] mdl
       [ Button.fab
       , Options.onClick (ToggleLight dimmer light)
       , Button.icon
       , state
       ]
       [ Icon.i "highlight"]

toggleBtn: Material.Model -> Dimmer -> Html Msg
toggleBtn mdl dimmer =
    let
        state =
            if dimmer.undrawn then Button.colored else Options.nop
    in
    Button.render Mdl [0, DimmerM.getId dimmer] mdl
      [ Options.css "align" "center"
      , Options.onClick (UndrawDimmer dimmer)
      , state
      ]
      [ text "Pokaż lampy"]
