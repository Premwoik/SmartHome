module Page.Dashboard.Light exposing (..)

import Page.Dashboard.Type exposing (..)

import Material.Card as Card
import Material.Button as Button
import Material.Options as Options exposing (css)
import Material.Icon as Icon
import Material


import Data.Light as Light exposing (Light)



lightCard : Material.Model -> Light -> Card.Block Msg
lightCard mdl light =
     Card.actions
            []
            [ Options.div
                  []
                  [ Card.subhead
                          [ css "display" "flex"
                          , css "align-items" "center"
                          , css "padding" ".3rem 2.5rem"
                          ]
                          [
                            Button.render Mdl [0] mdl
                                    [ Options.onClick (LightToggle light)
                                    , Button.fab
                                    , Options.when (light.state == True) Button.colored
                                    ]
                                    [ Icon.i "highlight"]
                          ]
                  ]
            ]