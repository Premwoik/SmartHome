module Page.Dashboard.Dimmer exposing (..)

import Page.Dashboard.Type exposing (..)

import Material.Card as Card
import Material.Button as Button
import Material.Options as Options exposing (css)
import Material.Icon as Icon
import Material.Color as Color
import Material.Typography as Typography
import Material.Slider as Slider
import Material

import Html exposing (..)
import Data.Dimmer exposing (Dimmer)


dimmerCard : Material.Model -> Dimmer-> Int -> Card.Block Msg
dimmerCard mdl dimmer raise=
    let
        cell =
            css "width" "64px"
        color =
          Color.color Color.Amber Color.S500
        icon =
            "wb_sunny"

        lightRow light=
              Card.subhead
                    [ css "display" "flex"
                    , css "align-items" "center"
                    , css "padding" ".3rem 2rem"
                    ]
                    [ Options.span [ cell ] [ text light.name ]
                    , Options.span [ cell, css "text-align" "center" ]
                        [ Icon.view icon [ Color.text color, Icon.size18 ] ]
                    , Options.span [ cell, css "text-align" "right" ]
                        [ Button.render Mdl [0] mdl
                             [ Button.fab
                             , Options.onClick (DimLightToggle dimmer light)
                             , Options.when light.state Button.colored
                             , Button.icon
--                             , state
                             ]
                             [ Icon.i "highlight"]
                        ]
                    ]

        renderLights =
               Options.div
                [ css "padding-top" "2rem"
                , css "margin" "auto"
                ]
                ( Options.span
                     [ Typography.title
                     , Color.text Color.primary
                     , css "margin-bottom" "3rem"
                     ]
                     [ text "Światła:"]
                :: List.map lightRow dimmer.lights)
    in
    Card.actions []
        [ Options.div
             []
             [ Card.subhead
                 [ css "display" "flex"
                 , css "align-items" "center"
                 , css "padding" ".3rem 2.5rem"
                 ]
                 [ Button.render Mdl [0] mdl
                         [ Button.fab
                         , Options.css "align" "center"
                         , Options.onClick (DimmerToggle dimmer)
                         , Options.when (dimmer.fill > 0) Button.colored
                         ]
                         [ Icon.i "highlight"]
                 ]
             , Slider.view
                [ Slider.step 25
                , Slider.value dimmer.fill
                , Slider.min 0
                , Slider.max 100
                , css "margin-top" "2rem"
                , Slider.onChange (SetDimmerFill dimmer)
--                        , (Options.when (not(Light.isOn light)) Slider.disabled)
                ]

             , (if List.length dimmer.lights > 0 then renderLights else div [] [])

             ]
        ]