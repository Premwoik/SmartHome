module Data.Dimmer exposing (..)
import Data.Light as Light
import Data.Id as Id
import Json.Decode as Decode exposing (nullable, map4, field, string, float, list, bool, Decoder)


type alias Dimmer =
    { id : Id.Id
    , name : String
    , fill : Float
    , lights : List Light.Light
    , undrawn : Bool
    }

type alias DimmerJson =
    { id : Id.Id
    , name : String
    , fill : Float
    , lights : List Light.Light
    }

decoder : Decoder DimmerJson
decoder =
    map4 DimmerJson (field "id" Id.decoder)
        (field "name" string)
        (field "fill" float)
        (field "lights" (list Light.decoder))


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

fromJson : DimmerJson -> Dimmer
fromJson dim =
    let
        cast = Dimmer dim.id dim.name dim.fill dim.lights
        defaultUndrawn = False
    in
    cast defaultUndrawn

updateFromJson : DimmerJson -> Dimmer -> Dimmer
updateFromJson dim ext =
    {ext | id = dim.id, name = dim.name, fill = dim.fill, lights = dim.lights}

getId : Dimmer -> Int
getId sun =
    Id.toInt sun.id

setFill : Dimmer -> Float -> Dimmer
setFill dim fill =
    {dim | fill = fill}

setOn : Dimmer -> Dimmer
setOn dim =
    {dim | lights = Light.setOnList dim.lights}

setOff : Dimmer -> Dimmer
setOff dim =
    {dim | lights = Light.setOffList dim.lights}

isOn : Dimmer -> Bool
isOn dim =
    List.any Light.isOn dim.lights

toggle : Dimmer -> Dimmer
toggle dim =
    case isOn dim of
        True -> setOff dim
        _ -> setOn dim

toggleUndrawn : Dimmer -> Dimmer
toggleUndrawn dim =
    {dim | undrawn = not dim.undrawn}

