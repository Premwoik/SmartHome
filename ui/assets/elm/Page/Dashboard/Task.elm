module Page.Dashboard.Task exposing (..)

import Page.Dashboard.Type exposing (..)

import Material.Card as Card
import Material.Button as Button
import Material.Options as Options exposing (css)
import Material.Icon as Icon
import Material.Color as Color
import Material.Toggles as Toggles
import Material.Typography as Typography
import Material

import Html exposing (..)

import Data.Task as Task exposing (Task)

taskCard : Material.Model -> Task -> Card.Block Msg
taskCard mdl task =
    let
        _ = ""
        icon = if task.status == Task.Inactive then "power_off" else "power"

    in
    Card.actions
        []
        [ Options.div
             []
              [ Options.span [Typography.title] [text "Status: "]
              , Options.span [Typography.body2, css "padding-left" "1rem"] [text (toString task.status)]
              , Options.span [Typography.title] [text "Typ: "]
              , Options.span [Typography.body2, css "padding-left" "1rem"] [text task.type_]
              , Card.subhead
                      [ css "display" "flex"
                      , css "justify-content" "space-between"
                      , css "align-items" "center"
                      , css "padding" ".3rem 2.5rem"
                      ]
                      [
                         Button.render Mdl [0] mdl
                                     [ Options.onClick (TaskToggle task)
                                     , Button.fab
                                     , Options.when (task.status /= Task.Inactive) Button.colored
                                     ]
                                     [ Icon.i icon]
                      ]
              ]
        ]