module Admin.Action exposing (..)

import Admin.Port as Port exposing (Port)
import Time exposing (Time)
import Html exposing (..)
import Material
import Material.Textfield as Textfield
import Material.Options as Options


import Json.Decode as Decode exposing (bool, field, int, nullable, string)
import Json.Decode.Pipeline as DP exposing (decode, required)
import Json.Encode as Encode

import Http
import Request as API exposing (url)

type alias Action =
    { id : Int
    , function : String
    , active : Bool
    , params : String
    , frequency : Int
    , startTime : Maybe String
    , endTime : Maybe String
    , portId : Maybe Int
--    , port_ : Maybe Port
    }

empty : Action
empty =
    Action 0 "" False "{}" 1000 Nothing Nothing Nothing


decoder : Decode.Decoder Action
decoder =
    decode Action
    |> required "id" int
    |> required "function" string
    |> required "active" bool
    |> required "params" string
    |> required "frequency" int
    |> required "start_time" (nullable string)
    |> required "end_time" (nullable string)
    |> required "port_id" (nullable int)
--    |> required "port" (nullable Port.decoder)

encode : Action -> Encode.Value
encode a =
    let
        optional e v =
            v |> Maybe.map e >> Maybe.withDefault Encode.null
    in
    Encode.object
        [ ("id", Encode.int a.id)
        , ("function", Encode.string a.function)
        , ("active", Encode.bool a.active)
        , ("params", Encode.string a.params)
        , ("frequency", Encode.int a.frequency)
        , ("start_time", optional Encode.string a.startTime)
        , ("end_time", optional Encode.string a.endTime)
        , ("port_id", optional Encode.int a.portId)
        ]
-- View

view : Html msg
view =
    Options.div []
        [
        ]


-- API

getActions : Http.Request (List Action)
getActions =
    let
        url_ = url ++ "actions"
        decode = Decode.at ["data"] (Decode.list decoder)
    in
    Http.get url_ decode

updateAction : Action -> Http.Request Action
updateAction a =
    let
        url_ = url ++ "actions/" ++ (toString a.id)
        decode = Decode.at ["data"] decoder
        body = Http.jsonBody (Encode.object [ ("action", encode a) ])
    in
    API.put url_  body decode

createAction : Action -> Http.Request Action
createAction a =
    let
        url_ = url ++ "actions/"
        decode = Decode.at ["data"] decoder
        body = Http.jsonBody (Encode.object [ ("action", encode a) ])
    in
    API.post url_  body decode


deleteAction: Int -> Http.Request ()
deleteAction a =
    let
        url_ = url ++ "actions/" ++ (toString a)
    in
    API.delete url_