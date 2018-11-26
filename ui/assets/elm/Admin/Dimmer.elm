module Admin.Dimmer exposing (..)


import Admin.Port as Port exposing (Port)
import Http
import Json.Decode.Pipeline as DP exposing (decode, required)
import Json.Decode as Decode exposing (int)
import Request as API exposing (url)
import Json.Encode as Encode


type alias Dimmer =
    { id : Int
    , fill : Int
    , direction : Int
    , fullTime : Int
    , port_ : Port
    }

empty : Dimmer
empty =
    let
        empty = Port.empty
        port_ = {empty | type_ = "dimmer", mode = "output"}
    in
    Dimmer 0 0 0 0 port_


decoder : Decode.Decoder Dimmer
decoder =
    decode Dimmer
        |> required "id" int
        |> required "fill" int
        |> required "direction" int
        |> required "full_time" int
        |> required "port" Port.decoder

encode : Dimmer -> Encode.Value
encode d =
    Encode.object
    [ ("id", Encode.int d.id)
    , ("fill", Encode.int d.fill)
    , ("direction", Encode.int d.direction)
    , ("time", Encode.int d.fullTime)
    , ("port", Port.encode d.port_)
    ]


-- API

getDimmers : Http.Request (List Dimmer)
getDimmers =
    let
        url_ = url ++ "dimmers"
        decode = Decode.at ["data"] (Decode.list decoder)
    in
    Http.get url_ decode

updateDimmer : Dimmer -> Http.Request Dimmer
updateDimmer d =
   let
       url_ = url ++ "dimmers/" ++ (toString d.id)
       decode = Decode.at ["data"] decoder
       body = Http.jsonBody (Encode.object [ ("dimmer", encode d) ])
   in
   API.put url_  body decode

createDimmer : Dimmer -> Http.Request Dimmer
createDimmer d =
   let
       url_ = url ++ "dimmers/"
       decode = Decode.at ["data"] decoder
       body = Http.jsonBody (Encode.object [ ("dimmer", encode d) ])
   in
   API.post url_  body decode

deleteDimmer: Int -> Http.Request ()
deleteDimmer d =
       let
           url_ = url ++ "dimmers/" ++ (toString d)
       in
       API.delete url_