module Request.Room exposing (..)

import Json.Decode as Decode exposing (map3, field, int, list, bool, string)
import Http
import Data.Dimmer as Dimmer
import Data.Sunblind as Sunblind
import Model exposing (..)

url = "http://0.0.0.0:4000/"

loadSunblinds : Cmd Msg
loadSunblinds =
    let
        url_ = url ++ "sunblinds"
        decoder = list Sunblind.decoder
    in
    Http.send InitSunblinds (Http.get url_ decoder)

loadDimmers : Cmd Msg
loadDimmers =
    let
            url_ = url ++ "dimmers"
            decoder = list Dimmer.decoder
        in
        Http.send InitDimmers (Http.get url_ decoder)

