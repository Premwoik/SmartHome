module Views.Card exposing (..)

import Html exposing (..)
import Material.Card as Card
import Material.Color as Color
import Material.Options as Options exposing (css)
import Material.Typography as Typography
import Material.Elevation as Elevation
import Material.Icon as Icon
import Model exposing (..)

card : String -> String -> Int -> Int -> Card.Block Msg -> Html Msg
card type_ name k raised action=
    Card.view
            [ css "width" "220px"
            , css "margin" ".5rem"
            , dynamic k raised
            ]
            [ Card.title
                [ css "padding" "1rem 1rem 0 1rem"]
                [ Card.head [] [text type_]
                , Options.div
                            [ css "padding" "0rem 2rem 0 2rem" ]
                            [ Options.span
                                [ Typography.display2
                                , Color.text Color.primary
                                ]
                                [ text name ]
                            ]
                ]
                , action
            ]


dynamic : Int -> Int -> Options.Style Msg
dynamic k raised =
  [ if k == raised then Elevation.e8 else Elevation.e2
  , Elevation.transition 250
  , Options.onMouseEnter (Raise k)
  , Options.onMouseLeave (Raise -1)
--  , Options.onClick showcode
  ] |> Options.many