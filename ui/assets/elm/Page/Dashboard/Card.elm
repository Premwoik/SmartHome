module Page.Dashboard.Card exposing (..)

import Page.Dashboard.Type exposing (..)


import Data.Dashboard as Dashboard exposing (Content)

import Html exposing (..)
import Material
import Material.Icon as Icon
import Material.Card as Card
import Material.Button as Button
import Material.Options as Options exposing (css)
import Material.Color as Color
import Material.Toggles as Toggles
import Material.Elevation as Elevation
import Material.Typography as Typography

import Page.Dashboard.Action exposing (actionCard)
import Page.Dashboard.Dimmer exposing (dimmerCard)
import Page.Dashboard.Light exposing (lightCard)
import Page.Dashboard.Port exposing (portCard)
import Page.Dashboard.Sunblind exposing (sunblindCard)
import Page.Dashboard.Task exposing (taskCard)

genCard : Material.Model -> Int -> Content -> Int -> Html Msg
genCard mdl i content raise =
    case content of
        Dashboard.Dimmer d ->
            card  "sciemniacz" d.name i raise (dimmerCard mdl d raise) (DimmerEdit d)
        Dashboard.Sunblind s ->
            card  "roleta" s.name i raise (sunblindCard mdl s) (SunblindEdit s)
        Dashboard.Light l ->
            card  "światło" l.name i raise (lightCard mdl l) (LightEdit l)
        Dashboard.Action a ->
            card  "akcja" a.name i raise (actionCard mdl a) (ActionEdit a)
        Dashboard.Port p ->
            card  (p.type_ ++ "*") p.name i raise (portCard mdl p) (PortEdit p)
        Dashboard.Task t ->
            card  "zadanie" t.name i raise (taskCard mdl t) (TaskEdit t)


card : String -> String -> Int -> Int -> (Card.Block Msg) -> Msg ->  Html Msg
card type_ name k raised action menuMsg=
    Card.view
            [ css "width" "100%"
            , css "margin-bottom" "1rem"
            , css "object-fit" "contain"
            , css "padding-bottom" "1rem"
            , dynamic k raised
            , Options.onClick Skip
            ]
            [ Card.menu [] [
--                  Button.render Mdl [0, k] model.mdl
--                                          [ Button.icon
--                                          , Options.onClick menuMsg
--                                          , Button.ripple
--                                          ]
--                                          [ Icon.i "edit" ]
                  ]
            , Card.title
                [ css "padding" "1rem 1rem 0 1rem"]
                [ Card.head [] [text type_]
            , Options.div
                        [ css "padding" "0rem 2rem 0 2rem" ]
                        [ Options.span
                            [ Typography.display1
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
--      , Options.onClick showcode
    ] |> Options.many

cardActionCss =
    [ css "display" "flex"
    , css "flex-direction" "column"
--    , css "margin-top" "20rem"
    ]