module Page.Dashboard.ActionCard exposing (card, stateToStr, view)

import Action exposing (Action)
import Bootstrap.Button as Button
import Bootstrap.Card as Card
import Bootstrap.Card.Block as Block
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Text as Text
import Html exposing (..)
import Html.Attributes as Attr exposing (style)
import Html.Events exposing (on, onClick)
import Json.Decode as Decode
import Page.Utils exposing (..)
import Port exposing (Port)


stateToStr : Action -> String
stateToStr { state } =
    case state of
        True ->
            "Wł."

        False ->
            "Wył."


view : (Action -> msg) -> Action -> Html msg
view msg action =
    card msg action
        |> Card.view


card : (Action -> msg) -> Action -> Card.Config msg
card msg action =
    let
        stateToColor =
            if action.state then
                Button.outlineSuccess

            else
                Button.outlineSecondary
    in
    Card.config [ Card.attrs [ style "width" "300px" ], Card.dark, Card.textColor Text.white ]
        |> Card.header [ style "padding" "5px", style "font-size" "15px" ] [ makeText [ "Akcja", action.name ] ]
        |> Card.block
            [ Block.attrs [ style "padding" "5px 5px 5px 5px" ]
            , Block.align Text.alignXsCenter
            ]
            [ Block.custom <|
                -- info
                Grid.container [ style "margin" "0 0 5px 0", style "font-size" "14px" ]
                    [ Grid.row []
                        [ Grid.col [] [ makeText [ "Status", ":", stateToStr action ] ]
                        , Grid.col [] [ makeText [ "Id", ":", String.fromInt action.id ] ]
                        ]
                    , Grid.row []
                        [ Grid.col [] [ makeText [ "Funkcja", ":", action.function ] ]
                        ]
                    ]
            , Block.custom <|
                Button.button [ stateToColor, Button.onClick (msg action) ] [ text "⛯" ]
            ]
        |> Card.footer [ style "padding" "5px", style "font-size" "10px" ] [ text "16 sec ago" ]
