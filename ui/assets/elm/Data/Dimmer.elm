module Data.Dimmer exposing (..)
import Data.Id as Id
import Json.Decode as Decode exposing (nullable, map5, field, string, float, list, bool, Decoder)


import Http
import Request
import Json.Encode as Encode
import Request exposing (data, refBody)

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
    (.lights >> List.any .state <| dim) || dim.fill > 0


-- API


setFill : Float -> Maybe Int -> Dimmer-> Http.Request Dimmer
setFill fill sRef d=
    let
        url_ = Request.url ++ "dimmers/setBrightness"
        data_ =
            [ ("id", Encode.int d.id)
            , ("fill", Encode.int (round fill))
            ]
    in
    Http.post url_ (refBody sRef data_) (data decoder)

setOn : Maybe Int -> Dimmer -> Http.Request Dimmer
setOn sRef d =
   let
        url_ = Request.url ++ "dimmers/setOn/" ++ (toString d.id)
    in
    Http.post url_ (refBody sRef []) (data decoder)

setOff : Maybe Int -> Dimmer -> Http.Request Dimmer
setOff sRef d =
   let
        url_ = Request.url ++ "dimmers/setOff/" ++ (toString d.id)
    in
    Http.post url_ (refBody sRef []) (data decoder)

toggle : Maybe Int -> Dimmer ->  Http.Request Dimmer
toggle sRef d=
    if isOn d then setOff sRef d else setOn sRef d

setLightOn : Maybe Int -> Light -> Http.Request Dimmer
setLightOn sRef l =
    let
         url_ = Request.url ++ "dimmers/setLightOn/" ++ (toString l.id)
     in
     Http.post url_ (refBody sRef []) (data decoder)

setLightOff : Maybe Int -> Light -> Http.Request Dimmer
setLightOff sRef l =
  let
       url_ = Request.url ++ "dimmers/setLightOff/" ++ (toString l.id)

   in
   Http.post url_ (refBody sRef  []) (data decoder)

toggleLight : Maybe Int-> Light -> Http.Request Dimmer
toggleLight sRef l=
    if l.state then setLightOff sRef l else setLightOn sRef l

getView : Int -> Http.Request Dimmer
getView id =
    let
        url_ = Request.url ++ "dimmers/cardView/" ++ (toString id)
    in
    Http.get url_ (data decoder)