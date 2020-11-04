module Page.Dashboard.LightCard exposing (card, view)

import Bootstrap.Button as Button
import Bootstrap.Card as Card
import Bootstrap.Card.Block as Block
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Text as Text
import Html exposing (..)
import Html.Attributes exposing (style)
import Light exposing (Light)
import Page.Utils exposing (..)
import Port exposing (Port)


on =
    "Wł."


off =
    "Wył."


status =
    "Status"


type_ =
    "Typ"


stateToStr : Light -> String
stateToStr { port_ } =
    case port_.state of
        True ->
            on

        False ->
            off


whatType : Light -> String
whatType { dimmer_id } =
    case dimmer_id of
        Nothing ->
            "zwykłe"

        _ ->
            "ściemniane"


view : (Light -> msg) -> Light -> Html msg
view msg light =
    card msg light
        |> Card.view


card : (Light -> msg) -> Light -> Card.Config msg
card msg light =
    let
        stateToColor =
            if light.port_.state then
                Button.outlineWarning

            else
                Button.outlineSecondary
    in
    Card.config [ Card.attrs [ style "width" "300px" ], Card.dark, Card.textColor Text.white ]
        |> Card.header [ style "padding" "5px", style "font-size" "15px" ] [ makeText [ "Światło", light.port_.name ] ]
        |> Card.block
            [ Block.attrs [ style "padding" "5px 5px 5px 5px" ]
            , Block.align Text.alignXsCenter
            ]
            [ Block.custom <|
                -- info
                Grid.container [ style "margin" "0 0 5px 0", style "font-size" "14px" ]
                    [ Grid.row []
                        [ Grid.col [] [ makeText [ status, ":", stateToStr light ] ]
                        , Grid.col [] [ makeText [ type_, ":", whatType light ] ]
                        ]
                    ]
            , Block.custom <|
                -- button
                Button.button [ stateToColor, Button.onClick (msg light) ] [ text "⛯" ]
            ]
        |> Card.footer [ style "padding" "5px", style "font-size" "10px" ] [ text "16 sec ago" ]
