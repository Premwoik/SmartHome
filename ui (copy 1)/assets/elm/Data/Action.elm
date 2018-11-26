module Data.Action exposing (..)
import Data.Id as Id exposing (Id)
import Data.Port as Port exposing (Port)
import Json.Decode as Decode exposing (field, bool, string, list, Decoder, int, at)

import Http
import Request
import Json.Encode as Encode



type alias Action =
    { id : Id
    , name : String
    , state : Bool
    , function : String
    , order: Int
    , action_ : String
    }

decoder : Decode.Decoder Action
decoder =
    Decode.map6 Action
        (field "id" Id.decoder)
        (field "name" string)
        (field "state" bool)
        (field "function" string)
        (field "order" int)
        (field "action" string)


-- API

setOn : Action -> Http.Request Action
setOn a =
    let
        url_ = Request.url ++ "actions/setOn"
        data = Encode.object
            [ ("id", Encode.int (Id.toInt a.id))
            ]
    in
    Http.post url_ (Http.jsonBody data) decoder

setOff : Action -> Http.Request Action
setOff a =
    let
        url_ = Request.url ++ "actions/setOff"
        data = Encode.object
            [ ("id", Encode.int (Id.toInt a.id))
            ]
    in
    Http.post url_ (Http.jsonBody data) decoder


toggle : Action -> Http.Request Action
toggle a =
   if a.state then setOff a else setOn a