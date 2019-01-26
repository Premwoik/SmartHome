module Data.Port exposing (..)

import Data.Id as Id exposing (Id)
import Json.Decode as Decode exposing (Decoder, map6, field, string, int, bool, andThen)


import Http
import Request exposing (data, refBody)
import Json.Encode as Encode


type alias Port =
    { id : Int
    , name : String
    , state : Bool
    , type_: String
--    , order : Int
    , port_ : String
    }

decoder : Decoder Port
decoder =
    Decode.map5 Port
        (field "id" int)
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

setOn : Maybe Int -> Port -> Http.Request Port
setOn sRef p =
    let
        url_ = Request.url ++ "ports/setOn/" ++ (toString p.id)
        dec = Decode.at ["data"] decoder

    in
    Http.post url_ (refBody sRef []) dec

setOff : Maybe Int -> Port -> Http.Request Port
setOff sRef p =
    let
        url_ = Request.url ++ "ports/setOff/" ++ (toString p.id)
        dec = Decode.at ["data"] decoder

    in
    Http.post url_ (refBody sRef []) dec


toggle : Maybe Int -> Port -> Http.Request Port
toggle sRef p=
   if p.state then setOff sRef p else setOn sRef p

getView : Int -> Http.Request Port
getView id =
  let
      url_ = Request.url ++ "ports/cardView/" ++ (toString id)
  in
  Http.get url_ (data decoder)

