module Data.Action exposing (..)
import Data.Id as Id exposing (Id)
import Data.Port as Port exposing (Port)
import Json.Decode as Decode exposing (field, bool, string, list, Decoder, int, at)

type alias Action =
    { id : Id
    , state : Bool
    , function : Function
    , activator : String
--    , args : List Port
    , params : String
    }

decoder : Decode.Decoder Action
decoder =
    Decode.map5 Action
        (field "id" Id.decoder)
        (field "active" bool)
        (field "function" functionDecoder)
        (at ["port", "name"] string)
--        (field "args" (list Port.decoder))
        (field "params" string)


type Function
     = Function String

functionDecoder : Decoder Function
functionDecoder =
    string
        |> Decode.andThen (\func -> Decode.succeed (Function func))

fnToString : Function -> String
fnToString (Function str) =
    str
