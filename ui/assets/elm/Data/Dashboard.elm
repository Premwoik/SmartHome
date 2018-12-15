module Data.Dashboard exposing (..)
import Data.Id as Id exposing (Id)
import Data.Light as Light exposing (Light)
import Data.Dimmer as Dimmer exposing (Dimmer)
import Data.Port as Port exposing (Port)
import Data.Action as Action exposing (Action)
import Data.Sunblind as Sunblind exposing (Sunblind)
import Data.Task as Task exposing (Task)

import Http
import Json.Decode as Decode exposing (field, map4, map3, map, string, map7, Decoder, array)
import Array exposing (Array)

import Request exposing (data)

type alias DashboardTabs = List DashboardShort


type alias DashboardShort =
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
    | Task Task


type alias Dashboard =
    {
    id : Id,
    number: Int,
    name: String,
    title: String,
    description: String,
    content: Array Content
    }

decoderShort : Decoder DashboardShort
decoderShort =
    Decode.map3 DashboardShort (field "id" Id.decoder)
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
        , map Task Task.decoder
        ]

decoder : Decoder Dashboard
decoder =
    Decode.map6 Dashboard (field "id" Id.decoder)
    (field "order" Decode.int)
    (field "name" Decode.string)
    (field "title" Decode.string)
    (field "description" Decode.string)
    (field "content" (Decode.array decoderContent))

empty : Dashboard
empty =
    Dashboard (Id.Id -1) -1 "" "" "" Array.empty


--    API


getTabs : Http.Request (List DashboardShort)
getTabs =
    let
        url_ = Request.url ++ "dashboards/view/short"
        decoder =  Decode.list decoderShort
    in
    Http.get url_ (data decoder)


getDashboard : Int -> Http.Request Dashboard
getDashboard id =
  let
        url_ = Request.url ++ "dashboards/view/" ++ (toString id)
    in
    Http.get url_  (data decoder)