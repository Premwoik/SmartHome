module Page.Dashboard.Action exposing (..)

import Page.Dashboard.Type exposing (..)

import Material.Card as Card
import Material.Button as Button
import Material.Options as Options exposing (css)
import Material.Icon as Icon
import Material.Color as Color
import Material.Toggles as Toggles
import Material.Typography as Typography
import Material

import Html exposing (..)

import Data.Action as Action exposing (Action)
actionCard : Material.Model -> Action -> Card.Block Msg
actionCard mdl action =
    let
        _ = ""
    in
    Card.actions
        [css "display" "flex"
        , css "flex-direction" "column"
        ]

        [ Options.span [Typography.title] [text "Funkcja: "]
        , Options.span [Typography.body2, css "padding-left" "1rem"] [text action.function]
        , Card.subhead
           [ css "display" "flex"
           , css "align-items" "center"
           , css "padding" ".3rem 2.5rem"
           ]
           [ Button.render Mdl [0] mdl
                   [ Button.fab
                   , Options.css "align" "center"
                   , Options.onClick (ActionToggle action)
                   , Options.when action.state Button.colored
                   ]
                   [ Icon.i "star"]
           ]
        ]
