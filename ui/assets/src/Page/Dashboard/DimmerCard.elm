module Page.Dashboard.DimmerCard exposing (card, view)

import Bootstrap.Button as Button
import Bootstrap.Card as Card
import Bootstrap.Card.Block as Block
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Text as Text
import Dimmer exposing (Dimmer)
import Html exposing (..)
import Html.Attributes as Attr exposing (style)
import Html.Events exposing (on, onClick)
import Json.Decode as Decode
import Page.Utils exposing (..)
import Port exposing (Port)


onChange : (String -> msg) -> Html.Attribute msg
onChange handler =
    on "change" <| Decode.map handler <| Decode.at [ "target", "value" ] Decode.string


slider : Dimmer -> (Dimmer -> msg) -> Html msg
slider d msg =
    input
        [ onChange (\x -> msg { d | fill = Maybe.withDefault 0 <| String.toInt x })
        , style "margin-top" "15px"
        , Attr.type_ "range"
        , Attr.min "0"
        , Attr.max "100"
        , Attr.step "25"
        , Attr.value <| String.fromInt d.fill
        ]
        []


view : (Dimmer -> msg) -> Dimmer -> Html msg
view msg dimmer =
    card msg dimmer
        |> Card.view


card : (Dimmer -> msg) -> Dimmer -> Card.Config msg
card msg dimmer =
    let
        stateToColor =
            if dimmer.fill > 0 then
                Button.outlineWarning

            else
                Button.outlineSecondary

        toggleDimmer =
            if dimmer.fill > 0 then
                { dimmer | fill = 0 }

            else
                { dimmer | fill = 100 }
    in
    Card.config [ Card.attrs [ style "width" "300px" ], Card.dark, Card.textColor Text.white ]
        |> Card.header [ style "padding" "5px", style "font-size" "15px" ] [ makeText [ "Ściemniacz", dimmer.port_.name ] ]
        |> Card.block
            [ Block.attrs [ style "padding" "5px 5px 5px 5px" ]
            , Block.align Text.alignXsCenter
            ]
            [ Block.custom <|
                -- info
                Grid.container [ style "margin" "0 0 5px 0", style "font-size" "14px" ]
                    [ Grid.row []
                        [ Grid.col [] [ makeText [ "Id", ":", String.fromInt dimmer.id ] ]
                        , Grid.col [] [ makeText [ "Jasność", ":", String.fromInt dimmer.fill, "%" ] ]
                        ]
                    , Grid.row []
                        [ Grid.col []
                            [ Button.button [ stateToColor, Button.onClick (msg dimmer) ] [ text "⛯" ] ]
                        , Grid.col []
                            [ slider dimmer msg ]
                        ]
                    ]
            ]
        |> Card.footer [ style "padding" "5px", style "font-size" "10px" ] [ text "16 sec ago" ]
