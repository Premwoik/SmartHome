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
import Json.Encode as Encode
import Array exposing (Array)

import Request exposing (data)
import Json.Decode.Pipeline as DP exposing (required, decode, hardcoded)

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

type alias ShortContent =
    { type_: String
    , id: Int
    }


type alias Dashboard =
    { id : Id
    , number: Int
    , name: String
    , title: String
    , description: String
    , content: List Content
    , shortContent: List ShortContent
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
    decode Dashboard
    |> required "id" Id.decoder
    |> required "order" Decode.int
    |> required "name" Decode.string
    |> required "title" Decode.string
    |> required "description" Decode.string
    |> required "content" (Decode.list decoderContent)
    |> hardcoded []

empty : Dashboard
empty =
    Dashboard (Id.Id -1) -1 "" "" "" [] []


--    API
makeShortContent : Dashboard -> Dashboard
makeShortContent d =
    let
        make : Content -> ShortContent
        make x =
            case x of
                Dimmer d -> ShortContent "dimmer" d.id
                Light l -> ShortContent "light" l.id
                Action a -> ShortContent "action" a.id
                Task t -> ShortContent "task" t.id
                Port p -> ShortContent "port" p.id
                Sunblind s -> ShortContent "sunblind" s.id

    in
    { d | shortContent = d.content |> List.map make}



updateContent : Content -> List Content -> List Content
updateContent c l =
    case c of
        Dimmer v ->
            let
                f x =
                    case x of
                        Dimmer v_ -> if v.id == v_.id then Dimmer v else Dimmer v_
                        x_ -> x_
            in
            List.map f l
        Light v ->
            let
                f x =
                    case x of
                        Light v_ -> if v.id == v_.id then Light v else Light v_
                        x_ -> x_
            in
            List.map f l
        Port v ->
            let
                f x =
                    case x of
                        Port v_ -> if v.id == v_.id then Port v else Port v_
                        x_ -> x_
            in
            List.map f l
        Action v ->
            let
                f x =
                    case x of
                        Action v_ -> if v.id == v_.id then Action v else Action v_
                        x_ -> x_
            in
            List.map f l
        Sunblind v ->
            let
                f x =
                    case x of
                        Sunblind v_ -> if v.id == v_.id then Sunblind v else Sunblind v_
                        x_ -> x_
            in
            List.map f l
        Task v ->
            let
                f x =
                    case x of
                        Task v_ -> if v.id == v_.id then Task v else Task v_
                        x_ -> x_
            in
            List.map f l


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


type alias UpdateNoti =
    { id: Int
    , type_ : String
    }

decodeUpdateNoti : Encode.Value -> Result String (List ShortContent)
decodeUpdateNoti raw =
    let
        objDecoder = Decode.map2 ShortContent (field "type" Decode.string) (field "id" Decode.int)
        decoder =  Decode.list objDecoder
    in
   Decode.decodeValue (data decoder) raw
