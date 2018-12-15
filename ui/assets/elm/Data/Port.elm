module Data.Port exposing (..)

import Data.Id as Id exposing (Id)
import Json.Decode as Decode exposing (Decoder, map6, field, string, int, bool, andThen)


import Http
import Request
import Json.Encode as Encode


type alias Port =
    { id : Id
    , name : String
    , state : Bool
    , type_: String
--    , order : Int
    , port_ : String
    }

decoder : Decoder Port
decoder =
    Decode.map5 Port
        (field "id" Id.decoder)
        (field "name" string)
        (field "state" bool)
        (field "type" string)
--        (field "order" int)
        (field "port" string)

type Type
    = Dimmer
    | DimLight
    | Light
    | Sunblind
    | Unknown String


-- API

setOn : Port -> Http.Request Port
setOn p =
    let
        url_ = Request.url ++ "ports/setOn/" ++ (toString (Id.toInt p.id))
        dec = Decode.at ["data"] decoder

    in
    Http.post url_ Http.emptyBody dec

setOff : Port -> Http.Request Port
setOff p =
    let
        url_ = Request.url ++ "ports/setOff/" ++ (toString (Id.toInt p.id))
        dec = Decode.at ["data"] decoder

    in
    Http.post url_ Http.emptyBody dec


toggle : Port -> Http.Request Port
toggle p =
   if p.state then setOff p else setOn p
