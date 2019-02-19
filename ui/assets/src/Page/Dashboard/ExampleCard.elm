module Page.Dashboard.ExampleCard exposing (card, view)

import Bootstrap.Card as Card
import Bootstrap.Card.Block as Block
import Html exposing (..)
import Html.Attributes exposing (href, style)


view : Html msg
view =
    card
        |> Card.view


card : Card.Config msg
card =
    Card.config
        [ Card.attrs [ style "width" "300px", style "max-height" "500px" ]
        , Card.outlineInfo
        ]
        |> Card.header [ style "font-size" "16px" ] [ text "ściemniacz - Salon telewizor" ]
        --|> Card.footer [] [ text "" ]
        |> Card.block []
            [ --, Block.text [] [ text "Some block content" ]
              Block.link [ href "#" ] [ text "MyLink" ]
            ]
