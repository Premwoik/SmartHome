module Page exposing (Config, Page(..), config, view, withContent, withPage)

import Bootstrap.Grid as Grid
import Browser exposing (Document)
import Html exposing (..)
import Html.Attributes exposing (style)
import Page.Dashboard as Dashboard
import Page.Navbar as Navbar
import Route exposing (href)


type alias Config msg =
    { navbarModel : Navbar.Model msg
    , selectedPage : Page
    , title : String
    , content : Html msg
    }


config : Navbar.Model msg -> Config msg
config m =
    { navbarModel = m
    , selectedPage = Unknown
    , title = "Unknown"
    , content = pageNotFound
    }


withContent : (subMsg -> msg) -> (model -> Html subMsg) -> model -> Config msg -> Config msg
withContent toMsg view_ model config_ =
    let
        subContent : Html msg
        subContent =
            Html.map toMsg (view_ model)
    in
    { config_ | content = subContent }


withPage : Page -> String -> Config msg -> Config msg
withPage page title config_ =
    { config_ | selectedPage = page, title = title }


type Page
    = Dashboard
    | Management
    | Unknown


view : Config msg -> Document msg
view c =
    { title = c.title ++ " - SmartHome"
    , body = Navbar.view c.navbarModel :: viewContent c.content :: [ viewFooter ]
    }


viewContent : Html msg -> Html msg
viewContent html =
    div
        [ style "max-width" "1600px"
        , style "margin" "auto"
        , style "padding" "1rem 0 0 0"
        ]
        [ html ]


viewFooter : Html msg
viewFooter =
    div [] []


pageNotFound : Html msg
pageNotFound =
    div []
        [ h1 [] [ text "Not found" ]
        , text "SOrry couldn't find that page"
        ]
