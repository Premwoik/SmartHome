module Data.Action exposing (..)
import Data.Id as Id exposing (Id)
import Data.Port as Port exposing (Port)
import Json.Decode as Decode exposing (field, bool, string, list, Decoder, int, at)

import Http
import Request exposing (data)
import Json.Encode as Encode




type alias Action =
    { id : Int
    , name : String
    , state : Bool
    , function : String
    , action_ : String
    }

decoder : Decode.Decoder Action
decoder =
    Decode.map5 Action
        (field "id" int)
        (field "name" string)
        (field "state" bool)
        (field "function" string)
        (field "action" string)


-- API

setOn : Action -> Http.Request Action
setOn a =
    let
        url_ = Request.url ++ "actions/setOn/" ++ (toString a.id)

    in
    Http.post url_ Http.emptyBody (data decoder)

setOff : Action -> Http.Request Action
setOff a =
    let
        url_ = Request.url ++ "actions/setOff/" ++ (toString a.id)

    in
    Http.post url_ Http.emptyBody (data decoder)


toggle : Action -> Http.Request Action
toggle a =
   if a.state then setOff a else setOn a