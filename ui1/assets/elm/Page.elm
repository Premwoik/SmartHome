module Page exposing (..)
import Html exposing (Html)
import Material.Options as Options exposing (css)

body1 : List (Html msg) -> Html msg
body1 =
     Options.div
        [ css "margin" "auto"
        , css "max-width" "1000px"
        , css "padding-left" "1%"
        , css "padding-right" "1%"
        , css "padding-top" "1%"
        , css "overflow-x" "auto"
        ]
