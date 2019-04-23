module Id exposing (Id(..), decoder, toInt)

import Json.Decode exposing (Decoder, int, map)


type Id
    = Id Int


decoder : Decoder Id
decoder =
    map Id int


toInt : Id -> Int
toInt (Id int) =
    int
