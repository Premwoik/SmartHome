module Page.Utils exposing (makeText)

import Html exposing (..)
import Http


makeText : List String -> Html msg
makeText l =
    text <| String.join " " l
