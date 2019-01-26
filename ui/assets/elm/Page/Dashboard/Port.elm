module Page.Dashboard.Port exposing (..)

import Page.Dashboard.Type exposing (..)

import Material.Card as Card
import Material.Button as Button
import Material.Options as Options exposing (css)
import Material.Icon as Icon
import Material.Color as Color
import Material.Toggles as Toggles
import Material

import Html exposing (..)

import Data.Port as Port exposing (Port)

portCard : Material.Model -> Port -> Card.Block Msg
portCard mdl port_ =
    let
        _ = ""
        icon = if port_.state == True then "power" else "power_off"

    in
    Card.actions
        []
        [ Options.div
            []
            [ Card.subhead
                  [ css "display" "flex"
                  , css "justify-content" "space-between"
                  , css "align-items" "center"
                  , css "padding" ".3rem 2.5rem"
                  ]
                  [
                     Button.render Mdl [0] mdl
                                 [ Options.onClick (PortToggle port_)
                                 , Button.fab
                                 , Options.when (port_.state == True) Button.colored
                                 ]
                                 [ Icon.i icon]
                  ]
            ]
        ]
