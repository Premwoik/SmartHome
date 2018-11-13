module Request.Room exposing (..)

import Json.Decode as Decode exposing (map3, field, int, list, bool, string)
import Json.Encode as Encode
import Http
import Data.Sunblind as Sunblind exposing (Sunblind)
import Data.Id as Id
import Data.Page as Page exposing (PageShort, Page)

import Request exposing (url)


loadSunblinds : Http.Request (List Sunblind)
loadSunblinds =
    let
        url_ = url ++ "sunblinds"
        decoder = list Sunblind.decoder
    in
    Http.get url_ decoder



toggleDimmer : Id.Id -> Http.Request String
toggleDimmer id=
    let
        url_ = url ++ "dimmers/toggle"
        decoder = string
        data = Encode.object
            [ ("id", Encode.int (Id.toInt id))
            ]
    in
    Http.post url_ (Http.jsonBody data) decoder

toggleDimLight : Id.Id -> Http.Request String
toggleDimLight id =
    let
        url_ = url ++ "dimmers/toggle_light"
        data = Encode.object
            [ ("id", Encode.int 0)
            , ("lightId", Encode.int (Id.toInt id))
            ]
    in
    Http.post url_ (Http.jsonBody data) string

toggleSunblind : Id.Id -> Http.Request String
toggleSunblind id =
    let
        url_ = url ++ "sunblinds/toggle_one"
        data = Encode.object
            [ ("id", Encode.int (Id.toInt id))
            ]
    in
    Http.post url_ (Http.jsonBody data) string

setDimFill : Id.Id -> Int -> Http.Request String
setDimFill id fill =
    let
        url_ = url ++ "dimmers/set_fill"
        data = Encode.object
            [ ("id", Encode.int (Id.toInt id))
            , ("fill", Encode.int fill)
            ]
    in
    Http.post url_ (Http.jsonBody data) string
