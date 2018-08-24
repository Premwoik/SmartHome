module Page.Room.Views.SunblindC exposing (sunblindCard)


import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Material
import Material.Tabs as Tabs
import Material.Slider as Slider
import Material.Scheme as Scheme
import Material.Card as Card
import Material.List as Lists
import Material.Layout as Layout
import Material.Button as Button
import Material.Textfield as Textfield
import Material.Toggles as Toggles
import Material.Color as Color
import Material.Options as Options exposing (css)
import Material.Typography as Typography
import Material.Elevation as Elevation
import Material.Icon as Icon
import Page.Room.Model exposing (..)

import Page.Room.Views.Card exposing (card)
import Data.Dimmer as DimmerM exposing (Dimmer)
import Data.Sunblind as SunblindM exposing (Sunblind)
import Data.Light as LightM exposing (Light)


sunblindCard : Material.Model -> List Sunblind -> Int -> Int -> Bool -> Html Msg
sunblindCard mdl sunblinds k raised toggled =
    let
        state = List.any (\x -> SunblindM.isOpen x) sunblinds
        action =
             Card.actions []
                        [ Options.div
                                 [ css "display" "flex"
                                 , css "flex-direction" "column"
                                 ]
                                 [ Card.subhead
                                         [ css "display" "flex"
                                         , css "align-items" "center"
                                         , css "padding" ".3rem 2.5rem"
                                         ]
                                         [ turningBtn mdl state
                                         ]
                                      , Card.subhead
                                         [ css "display" "flex"
                                         , css "align-items" "center"
                                         , css "padding" ".3rem 3.5rem"
                                         ]
                                         [ toggleMoreBtn mdl toggled
                                         ]
                                      , Options.div [] (renderSunblinds mdl sunblinds toggled)

                                 ]
                        ]
    in
    card "rolety" "All" k raised action


turningBtn : Material.Model -> Bool -> Html Msg
turningBtn mdl state =
   let
        state_ = if state then Button.colored else Options.nop
    in
    Button.render Mdl [0] mdl
      [ Button.fab
      , Options.css "align" "center"
      , Options.onClick SetAllSunblinds
      , state_
      ]
      [ Icon.i (icon_ state)]

toggleMoreBtn : Material.Model -> Bool -> Html Msg
toggleMoreBtn mdl toggled=
    let
        state =
            if toggled then Button.colored else Options.nop
    in
    Button.render Mdl [2, 0] mdl
      [ Options.onClick UndrawSunblinds
      , state
      ]
      [ text "Pokaż rolety"]


renderSunblinds : Material.Model -> List Sunblind -> Bool -> List (Html Msg)
renderSunblinds mdl list toggled =
    let
        render =
            List.map (\x -> row mdl x) list
    in
    if toggled then render else []

state sunblind =
       if SunblindM.isOpen sunblind then Button.colored else Options.nop

icon_ state =
    if state then "bookmark" else "bookmark_border"

icon sunblind =
    if SunblindM.isOpen sunblind then "bookmark" else "bookmark_border"


row : Material.Model -> Sunblind -> Html Msg
row mdl sunblind =
    let
        cell =
          css "width" "64px"

        color =
          Color.color Color.Amber Color.S500
    in
    Card.subhead
        [ css "display" "flex"
        , css "justify-content" "space-between"
        , css "align-items" "center"
        , css "padding" ".3rem 3rem"
        ]
        [ Options.span [ cell ] [ text sunblind.name ]
        , Options.span [ cell, css "text-align" "right" ]
            [ toggleSunblind mdl sunblind
            ]
        ]

toggleSunblind : Material.Model -> Sunblind -> Html Msg
toggleSunblind mdl sunblind =
        Button.render Mdl [2, SunblindM.getId sunblind] mdl
           [ Button.fab
           , Options.onClick (ToggleSunblind sunblind)
           , Button.icon
           , state sunblind
           ]
           [ Icon.i (icon sunblind)]

