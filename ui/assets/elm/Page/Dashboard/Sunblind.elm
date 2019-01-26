module Page.Dashboard.Sunblind exposing (..)


import Page.Dashboard.Type exposing (..)

import Material.Card as Card
import Material.Button as Button
import Material.Options as Options exposing (css)
import Material.Icon as Icon
import Material.Color as Color
import Material.Toggles as Toggles
import Material

import Html exposing (..)

import Data.Sunblind as Sunblind exposing (Sunblind)


sunblindCard : Material.Model -> Sunblind -> Card.Block Msg
sunblindCard mdl sunblind =
    let

        manual =
            sunblind.state == Sunblind.Position
        icon =
            case sunblind.state of
                Sunblind.Position -> "loop"
                Sunblind.InMove -> "block"
                Sunblind.Open -> "bookmark_border"
                Sunblind.Close -> "bookmark"

    in
    Card.actions
        []
        [ Options.div []
            [ Card.subhead
                [ css "display" "flex"
                , css "justify-content" "space-between"
                , css "align-items" "center"
                , css "padding" ".3rem 2.5rem"
                ]
                [ Button.render Mdl [0] mdl
                    [ Options.onClick (SunblindToggle sunblind)
                    , Button.fab
                    , Color.background (Color.color Color.Yellow Color.S300)
                    ]
                    [ Icon.i icon]
                ]
            , Toggles.switch Mdl [0] mdl
                [ Options.onToggle (SunblindManualToggle sunblind)
                , Toggles.ripple
                , Toggles.value manual
                ]
                [ text "Manual" ]
            ]
        ]