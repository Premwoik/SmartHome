module Data.Id exposing (..)
import Json.Decode exposing (int, map, Decoder)

type Id =
    Id Int


decoder : Decoder Id
decoder =
    map Id int

toInt : Id -> Int
toInt (Id int) =
    int