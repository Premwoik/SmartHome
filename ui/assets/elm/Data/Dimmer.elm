module Data.Dimmer exposing (..)
import Data.Id as Id
import Json.Decode as Decode exposing (nullable, map5, field, string, float, list, bool, Decoder)


import Http
import Request
import Json.Encode as Encode
import Request exposing (data)

type alias Light =
    { id : Int
    , name : String
    , state : Bool
    }

type alias Dimmer =
   { id : Int
      , name : String
      , fill : Float
      , lights : List Light
      , dimmer_ : String
   }

lightDecoder : Decoder Light
lightDecoder =
    Decode.map3 Light (field "id" Decode.int)
        (field "name" string)
        (field "state" bool)

decoder : Decoder Dimmer
decoder =
    Decode.map5 Dimmer (field "id" Decode.int)
        (field "name" string)
        (field "fill" float)
        (field "lights" (list lightDecoder))
        (field "dimmer" string)

-- DIRECTION

type Direction = Up | Down

directionDecoder : Decoder Direction
directionDecoder =
    bool
        |> Decode.andThen (\dir ->
            case dir of
                True -> Decode.succeed Up
                False -> Decode.succeed Down
            )


-- FUNCTIONS


isOn : Dimmer -> Bool
isOn dim =
    .lights >> List.any .state <| dim


-- API


setFill : Float -> Dimmer -> Http.Request Dimmer
setFill fill d=
    let
        url_ = Request.url ++ "dimmers/setFill"
        data = Encode.object
            [ ("id", Encode.int d.id)
            , ("fill", Encode.int (round fill))
            ]
    in
    Http.post url_ (Http.jsonBody data) decoder

setOn : Dimmer -> Http.Request Dimmer
setOn d =
   let
        url_ = Request.url ++ "dimmers/setOn/" ++ (toString d.id)
    in
    Http.post url_ Http.emptyBody (data decoder)

setOff : Dimmer -> Http.Request Dimmer
setOff d =
   let
        url_ = Request.url ++ "dimmers/setOff/" ++ (toString d.id)
    in
    Http.post url_ Http.emptyBody (data decoder)

toggle : Dimmer -> Http.Request Dimmer
toggle d =
    if isOn d then setOff d else setOn d

setLightOn : Light -> Http.Request Dimmer
setLightOn l =
    let
         url_ = Request.url ++ "dimmers/setLightOn/" ++ (toString l.id)
     in
     Http.post url_ Http.emptyBody (data decoder)

setLightOff : Light -> Http.Request Dimmer
setLightOff l =
  let
       url_ = Request.url ++ "dimmers/setLightOff/" ++ (toString l.id)

   in
   Http.post url_ Http.emptyBody (data decoder)

toggleLight : Light -> Http.Request Dimmer
toggleLight l =
    if l.state then setLightOff l else setLightOn l