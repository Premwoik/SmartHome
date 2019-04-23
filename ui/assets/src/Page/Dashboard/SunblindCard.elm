module Page.Dashboard.SunblindCard exposing (card, stateToStr, view)

import Bootstrap.Button as Button
import Bootstrap.ButtonGroup as ButtonGroup
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
import Sunblind exposing (State(..), Sunblind, isManual)


stateToStr : Sunblind -> String
stateToStr { state } =
    case state of
        Open ->
            "Otwarte"

        Close ->
            "Zamknięte"

        InMove ->
            "W ruchu"

        Position ->
            "Pozycja"


view : (Sunblind -> msg) -> (Sunblind -> msg) -> Sunblind -> Html msg
view tglState tglMode sunblind =
    card tglState tglMode sunblind
        |> Card.view


card : (Sunblind -> msg) -> (Sunblind -> msg) -> Sunblind -> Card.Config msg
card tglState tglMode sunblind =
    let
        stateToColor =
            case sunblind.state of
                Open ->
                    Button.outlineSecondary

                Close ->
                    Button.outlineSuccess

                InMove ->
                    Button.disabled True

                Position ->
                    Button.outlineWarning
    in
    Card.config [ Card.attrs [ style "width" "300px" ], Card.dark, Card.textColor Text.white ]
        |> Card.header [ style "padding" "5px", style "font-size" "15px" ] [ makeText [ "Rolety", sunblind.name ] ]
        |> Card.block
            [ Block.attrs [ style "padding" "5px 5px 5px 5px" ]
            , Block.align Text.alignXsCenter
            ]
            [ Block.custom <|
                -- info
                Grid.container [ style "margin" "0 0 5px 0", style "font-size" "14px" ]
                    [ Grid.row []
                        [ Grid.col [] [ makeText [ "Status", ":", stateToStr sunblind ] ]

                        --, Grid.col [] [ makeText [ "Id", ":", String.fromInt sunblind.id ] ]
                        ]
                    , Grid.row []
                        [ Grid.col [] [ makeText [ "Typ", ":", "spec" ] ]
                        , Grid.col []
                            [ Button.checkboxButton
                                (isManual sunblind)
                                [ Button.attrs [ style "padding" "0 5px 0 5px", style "font-size" "16px" ], Button.small, Button.outlineSecondary, Button.onClick (tglMode sunblind) ]
                                [ text "Manual" ]
                            ]

                        --Button.radioButton (isManual sunblind) [ Button.small, Button.primary, Button.onClick (tglMode sunblind) ] [ text "Manual" ] ]
                        ]
                    ]
            , Block.custom <|
                Button.button [ stateToColor, Button.onClick (tglState sunblind) ] [ text "⛯" ]
            ]
        |> Card.footer [ style "padding" "5px", style "font-size" "10px" ] [ text "16 sec ago" ]
