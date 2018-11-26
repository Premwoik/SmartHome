module Data.Page exposing (..)
import Data.Id as Id exposing (Id)
import Data.Light as Light exposing (Light)
import Data.Dimmer as Dimmer exposing (Dimmer)
import Data.Port as Port exposing (Port)
import Data.Action as Action exposing (Action)
import Data.Sunblind as Sunblind exposing (Sunblind)

import Json.Decode as Decode exposing (field, map4, map3, map, string, map7, Decoder, array)
import Array exposing (Array)

type alias PageShort =
    {
    id: Id,
    number: Int,
    name: String
    }

type Content
    = Dimmer Dimmer
    | Light Light
    | Port Port
    | Action Action
    | Sunblind Sunblind


type alias Page =
    {
    id : Id,
    number: Int,
    name: String,
    title: String,
    description: String,
    content: Array Content
    }

decoderShort : Decoder PageShort
decoderShort =
    Decode.map3 PageShort (field "id" Id.decoder)
    (field "number" Decode.int)
    (field "name" Decode.string)




decoderContent : Decoder Content
decoderContent =
    Decode.oneOf
        [ map Dimmer Dimmer.decoder
        , map Light Light.decoder
        , map Sunblind Sunblind.decoder
        , map Port Port.decoder
        , map Action Action.decoder
        ]

decoder : Decoder Page
decoder =
    Decode.map6 Page (field "id" Id.decoder)
    (field "number" Decode.int)
    (field "name" Decode.string)
    (field "title" Decode.string)
    (field "description" Decode.string)
    (field "content" (Decode.array decoderContent))

empty : Page
empty =
    Page (Id.Id -1) -1 "" "" "" Array.empty


--    API