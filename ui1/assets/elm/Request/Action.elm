module Request.Action exposing (..)

import Http
import Data.Action as Action exposing (Action)
import Data.Id as Id exposing (Id)
import Json.Encode as Encode
import Json.Decode as Decoder exposing (string, list)
import Request

getActions : Http.Request (List Action)
getActions =
    let
        url_ = Request.url ++ "actions"
        decoder = list Action.decoder
    in
    Http.get url_ decoder



toggleAction : Id -> Http.Request String
toggleAction id =
     let
            url_ = Request.url ++ "actions/toggle"
            data = Encode.object
                       [ ("id", Encode.int (Id.toInt id))
                       ]
     in
     Http.post url_ (Http.jsonBody data) string