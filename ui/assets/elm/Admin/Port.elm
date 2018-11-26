module Admin.Port exposing (..)

import Admin.Device as Device exposing (Device)

import Http
import Json.Encode as Encode
import Json.Decode as Decode exposing (bool, field, int, nullable, string)
import Json.Decode.Pipeline as DP exposing (decode, required, optional)
import Request as API exposing (url)

type alias Port =
    { id : Int
    , name : String
    , mode : String
    , state : Bool
    , type_ : String
    , timeout : Int
    , deviceId : Maybe Int
    , device : Maybe Device
    }

empty : Port
empty =
    Port 0 "" "" False "" 0 Nothing Nothing


decoder : Decode.Decoder Port
decoder =
    decode Port
        |> required "id" int
        |> required "name" string
        |> required "mode" string
        |> optional "state" bool False
        |> required "type" string
        |> required "timeout" int
        |> optional "device_id" (nullable int) Nothing
        |> optional "device" (nullable Device.decoder) Nothing

encode : Port -> Encode.Value
encode p =
    Encode.object
        [ ("id", Encode.int p.id)
        , ("name", Encode.string p.name)
        , ("mode", Encode.string p.mode)
        , ("state", Encode.bool p.state)
        , ("type", Encode.string p.type_)
        , ("timeout", Encode.int p.timeout)
--        , ("device_id", Encode.)
        ]

-- API

getPorts : Http.Request (List Port)
getPorts =
    let
        url_ = url ++ "ports"
        decode = Decode.at ["data"] (Decode.list decoder)
    in
    Http.get url_ decode

updatePort : Port -> Http.Request Port
updatePort p =
    let
        url_ = url ++ "ports/" ++ (toString p.id)
        decode = Decode.at ["data"] decoder
        body = Http.jsonBody (Encode.object [ ("port", encode p) ])
    in
    API.put url_  body decode

createPort : Port -> Http.Request Port
createPort p =
    let
        url_ = url ++ "ports/"
        decode = Decode.at ["data"] decoder
        body = Http.jsonBody (Encode.object [ ("port", encode p) ])
    in
    API.post url_  body decode

deletePort : Int -> Http.Request ()
deletePort p =
       let
           url_ = url ++ "ports/" ++ (toString p)
       in
       API.delete url_