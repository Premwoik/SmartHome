module Data.Light exposing (..) --(setOnList, setOffList, isOn, Light, setOff, setOn, toggle, decoder)
import Data.Id as Id
import Json.Decode as Decode exposing (field, bool, string, map5, Decoder, andThen)

import Http
import Request exposing (data, refBody)
import Json.Encode as Encode


type alias Light =
    {
        id : Int,
--        fill : Maybe Int,
--        order: Int,
        state : Bool,
        name : String,
        light_ : String
    }

decoder : Decoder Light
decoder =
    Decode.map4 Light (field "id" Decode.int)
--        (field "fill" (Decode.nullable Decode.int))
--        (field "order" Decode.int)
        (field "state" bool)
        (field "name" string)
        (field "light" string)

-- STATE

--type State =
--    On | Off
--
--notState : State -> State
--notState state =
--    case state of
--        On -> Off
--        Off -> On
--
--stateDecoder : Decoder State
--stateDecoder =
--    bool
--        |> Decode.andThen (\state ->
--            case state of
--                True -> Decode.succeed On
--                False -> Decode.succeed Off
--            )

-- FUNCTIONS

--unpackFill : Light -> Float
--unpackFill =
--    .fill >> Maybe.withDefault 0 >> toFloat

-- Api

setOn : Maybe Int -> Light -> Http.Request Light
setOn sRef l =
    let
        url_ = Request.url ++ "lights/setOn/" ++ (toString l.id)
    in
    Http.post url_ (refBody sRef []) (Decode.at ["data"] decoder)

setOff : Maybe Int -> Light -> Http.Request Light
setOff sRef l =
    let
        url_ = Request.url ++ "lights/setOff/" ++ (toString l.id)
    in
    Http.post url_ (refBody sRef []) (Decode.at ["data"] decoder)

--setFill : Float -> Light -> Http.Request Light
--setFill fill l =
--    let
--        url_ = Request.url ++ "lights/setFill"
--        data = Encode.object
--            [ ("id", Encode.int (Id.toInt l.id))
--            , ("fill", Encode.int (round fill))
--            ]
--    in
--    Http.post url_ (Http.jsonBody data) decoder

toggle : Maybe Int-> Light -> Http.Request Light
toggle sRef l=
   if l.state then setOff sRef l else setOn sRef l


getView : Int -> Http.Request Light
getView id =
  let
      url_ = Request.url ++ "lights/cardView/" ++ (toString id)
  in
  Http.get url_ (data decoder)

