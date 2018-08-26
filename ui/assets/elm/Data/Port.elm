module Data.Port exposing (..)

import Data.Id as Id exposing (Id)
import Json.Decode as Decode exposing (Decoder, map7, field, string, int, bool, andThen)

type alias Port =
    { id : Id
    , device_id : Id
    , name : String
    , type_ : Type
    , number : Int
    , timeout : Int
    , state : Bool
    }

decoder : Decoder Port
decoder =
    map7 Port
        (field "id" Id.decoder)
        (field "device_id" Id.decoder)
        (field "name" string)
        (field "type" typeDecoder)
        (field "number" int)
        (field "timeout" int)
        (field "state" bool)

type Type
    = Dimmer
    | DimLight
    | Light
    | Sunblind
    | Unknown String

typeDecoder : Decoder Type
typeDecoder =
    string |> andThen (\type_ ->
        case type_ of
            "dimmer" -> Decode.succeed Dimmer
            "dimLight" -> Decode.succeed Light
            "light" -> Decode.succeed Light
            "sunblind" -> Decode.succeed Sunblind
            other -> Decode.succeed (Unknown other)
        )
