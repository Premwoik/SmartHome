module Admin.Device exposing (..)

import Http
import Json.Decode as Decode exposing (field, int, string)
import Json.Decode.Pipeline as DP exposing (decode, required)
import Request as API exposing (url)
import Json.Encode as Encode

type alias Device =
    { id : Int
    , name : String
    , ip : String
    , port_ : Int
    , type_ : String
    }

empty : Device
empty =
    Device 0 "" "" 5000 ""

decoder : Decode.Decoder Device
decoder =
    decode Device
        |> required "id" int
        |> required "name" string
        |> required "ip" string
        |> required "port" int
        |> required "type" string

encode : Device -> Encode.Value
encode d =
    Encode.object
        [ ("id", Encode.int d.id)
        , ("name", Encode.string d.name)
        , ("ip", Encode.string d.ip)
        , ("port", Encode.int d.port_)
        , ("type", Encode.string d.type_)
        ]

-- API

getDevices: Http.Request (List Device)
getDevices =
   let
       url_ = url ++ "devices"
       decode = Decode.at ["data"] (Decode.list decoder)
   in
   Http.get url_ decode

updateDevice : Device -> Http.Request Device
updateDevice d =
       let
           url_ = url ++ "devices/" ++ (toString d.id)
           decode = Decode.at ["data"] decoder
           body = Http.jsonBody (Encode.object [ ("device", encode d) ])
       in
       API.put url_  body decode

createDevice: Device -> Http.Request Device
createDevice d =
       let
           url_ = url ++ "devices/"
           decode = Decode.at ["data"] decoder
           body = Http.jsonBody (Encode.object [ ("device", encode d) ])
       in
       API.post url_  body decode

deleteDevice: Int -> Http.Request ()
deleteDevice d =
       let
           url_ = url ++ "devices/" ++ (toString d)
       in
       API.delete url_


