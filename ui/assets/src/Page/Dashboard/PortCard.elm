module Page.Dashboard.PortCard exposing (card, view)

import Bootstrap.Button as Button
import Bootstrap.Card as Card
import Bootstrap.Card.Block as Block
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Text as Text
import Html exposing (..)
import Html.Attributes exposing (style)
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


stateToStr : Port -> String
stateToStr { state } =
    case state of
        True ->
            on

        False ->
            off


view : (Port -> msg) -> Port -> Html msg
view msg port_ =
    card msg port_
        |> Card.view


card : (Port -> msg) -> Port -> Card.Config msg
card msg port_ =
    let
        stateToColor =
            if port_.state then
                Button.outlineSuccess

            else
                Button.outlineSecondary
    in
    Card.config [ Card.attrs [ style "width" "300px" ], Card.dark, Card.textColor Text.white ]
        |> Card.header [ style "padding" "5px", style "font-size" "15px" ] [ text ("Port " ++ port_.name) ]
        |> Card.block
            [ Block.attrs [ style "padding" "5px 5px 5px 5px" ]
            , Block.align Text.alignXsCenter
            ]
            [ Block.custom <|
                -- info
                Grid.container [ style "margin" "0 0 5px 0", style "font-size" "14px" ]
                    [ Grid.row []
                        [ Grid.col [] [ makeText [ status, ":", stateToStr port_ ] ]
                        , Grid.col [] [ makeText [ type_, ":", port_.type_ ] ]
                        ]
                    ]
            , Block.custom <|
                -- button
                Button.button [ stateToColor, Button.onClick (msg port_) ] [ text "⛯" ]
            ]
        |> Card.footer [ style "padding" "5px", style "font-size" "10px" ] [ text "16 sec ago" ]
