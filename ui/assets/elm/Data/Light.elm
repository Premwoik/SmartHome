module Data.Light exposing (..) --(setOnList, setOffList, isOn, Light, setOff, setOn, toggle, decoder)
import Data.Id as Id
import Json.Decode as Decode exposing (field, bool, string, map5, Decoder, andThen)

import Http
import Request
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

setOn : Light -> Http.Request Light
setOn l =
    let
        url_ = Request.url ++ "lights/setOn/" ++ (toString l.id)
    in
    Http.post url_ Http.emptyBody (Decode.at ["data"] decoder)

setOff : Light -> Http.Request Light
setOff l =
    let
        url_ = Request.url ++ "lights/setOff/" ++ (toString l.id)
    in
    Http.post url_ Http.emptyBody (Decode.at ["data"] decoder)

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

toggle : Light -> Http.Request Light
toggle l =
   if l.state then setOff l else setOn l