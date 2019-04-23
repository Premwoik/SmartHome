module Route exposing (Route(..), fromUrl, href, routeToString)

import Html
import Html.Attributes as Attr
import Url exposing (Url)
import Url.Parser as UrlParser exposing ((</>), Parser, int, map, oneOf, s, string, top)


type Route
    = Management String
    | Dashboard Int
    | Home


href : Route -> Html.Attribute msg
href r =
    Attr.href (routeToString r)


routeToString : Route -> String
routeToString r =
    let
        pieces : List String
        pieces =
            case r of
                Management s ->
                    [ "management", s ]

                Dashboard id ->
                    [ "dashboard", String.fromInt id ]

                Home ->
                    []
    in
    "#/" ++ String.join "/" pieces


fromUrl : Url -> Maybe Route
fromUrl url =
    -- The RealWorld spec treats the fragment like a path.
    -- This makes it *literally* the path, so we can proceed
    -- with parsing as if it had been a normal path all along.
    { url | path = Maybe.withDefault "" url.fragment, fragment = Nothing }
        |> UrlParser.parse parser


parser : Parser (Route -> a) a
parser =
    oneOf
        [ map Home top
        , map Management (s "management" </> string)
        , map Dashboard (s "dashboard" </> int)
        ]
