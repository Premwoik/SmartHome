module Page.Dashboard.TaskCard exposing (card, view)

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
import TaskM exposing (Status(..), Task)



--stateToStr : Task -> String
--stateToStr { status } =
--case status of
--"inactive" ->
--"Wł."
--"waiting" ->
--"Wył."


view : (Task -> msg) -> Task -> Html msg
view msg task =
    card msg task
        |> Card.view


card : (Task -> msg) -> Task -> Card.Config msg
card msg task =
    let
        stateToColor =
            case task.status of
                Running ->
                    Button.outlineSuccess

                Waiting ->
                    Button.outlineWarning

                _ ->
                    Button.outlineSecondary
    in
    Card.config [ Card.attrs [ style "width" "300px" ], Card.dark, Card.textColor Text.white ]
        |> Card.header [ style "padding" "5px", style "font-size" "15px" ] [ makeText [ "Zadanie", task.name ] ]
        |> Card.block
            [ Block.attrs [ style "padding" "5px 5px 5px 5px" ]
            , Block.align Text.alignXsCenter
            ]
            [ Block.custom <|
                -- info
                Grid.container [ style "margin" "0 0 5px 0", style "font-size" "14px" ]
                    [ Grid.row []
                        [ Grid.col [] [ makeText [ "Status", ":", "Stat" ] ]
                        , Grid.col [] [ makeText [ "Id", ":", String.fromInt task.id ] ]
                        ]
                    , Grid.row []
                        [ Grid.col [] [ makeText [ "Funkcja", ":", task.type_ ] ]
                        ]
                    ]
            , Block.custom <|
                Button.button [ stateToColor, Button.onClick (msg task) ] [ text "⛯" ]
            ]
        |> Card.footer [ style "padding" "5px", style "font-size" "10px" ] [ text "16 sec ago" ]
