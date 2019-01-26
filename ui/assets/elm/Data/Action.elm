module Data.Action exposing (..)
import Data.Id as Id exposing (Id)
import Data.Port as Port exposing (Port)
import Json.Decode as Decode exposing (field, bool, string, list, Decoder, int, at)

import Http
import Request exposing (data, refBody)
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

setOn : Action -> Maybe Int -> Http.Request Action
setOn a sRef=
    let
        url_ = Request.url ++ "actions/setOn/" ++ (toString a.id)

    in
    Http.post url_ (refBody sRef []) (data decoder)

setOff : Action -> Maybe Int-> Http.Request Action
setOff a sRef=
    let
        url_ = Request.url ++ "actions/setOff/" ++ (toString a.id)
    in
    Http.post url_ (refBody sRef []) (data decoder)


toggle : Maybe Int-> Action -> Http.Request Action
toggle sRef a=
   if a.state then setOff a sRef else setOn a sRef


getView : Int -> Http.Request Action
getView id =
  let
      url_ = Request.url ++ "actions/cardView/" ++ (toString id)
  in
  Http.get url_ (data decoder)
