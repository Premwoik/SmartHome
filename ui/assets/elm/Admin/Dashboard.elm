module Admin.Dashboard exposing (..)

import Http
import Json.Decode as Decode
import Json.Encode as Encode
import Json.Decode.Pipeline as DP exposing (decode, required, hardcoded)
import Request as API exposing (url, data)


type alias Id = Int
type alias Order = Int
type alias Content = (Id, Order)

type alias Dashboard =
    { id : Int
    , name : String
    , title : String
    , description : String
    , order : Int
    , sunblinds : String
    , dimmers : String--List Content
    , lights : String --List Content
    , devices : String --List Content
    , actions : String --List Content
    , tasks : String --List Content
    , ports : String --List Content

    }

contentDecoder : Decode.Decoder Content
contentDecoder =
    Decode.map2 (,) (Decode.index 0 Decode.int)
        (Decode.index 1 Decode.int)

decoder : Decode.Decoder Dashboard
decoder =
    decode Dashboard
    |> required "id" Decode.int
    |> required "name" Decode.string
    |> required "title" Decode.string
    |> required "description" Decode.string
    |> required "order" Decode.int
    |> required "sunblinds" Decode.string
    |> required "dimmers" Decode.string--(Decode.list contentDecoder)
    |> required "lights" Decode.string --(Decode.list contentDecoder)
    |> required "devices" Decode.string --(Decode.list contentDecoder)
    |> required "actions" Decode.string --(Decode.list contentDecoder)
    |> required "tasks" Decode.string --(Decode.list contentDecoder)
    |> required "ports" Decode.string --(Decode.list contentDecoder)


encode : Dashboard -> Encode.Value
encode d =
    Encode.object
        [ ("id", Encode.int d.id)
        , ("name", Encode.string d.name)
        , ("title", Encode.string d.title)
        , ("description", Encode.string d.description)
        , ("order", Encode.int d.order)
        , ("sunblinds", Encode.string d.sunblinds)
        , ("dimmers", Encode.string d.dimmers)
        , ("lights", Encode.string d.lights)
        , ("devices", Encode.string d.devices)
        , ("actions", Encode.string d.actions)
        , ("tasks", Encode.string d.tasks)
        , ("ports", Encode.string d.ports)
        ]

empty : Dashboard
empty =
    Dashboard 0 "" "" "" 0 "" "" "" "" "" "" ""

getDashboards : Http.Request (List Dashboard)
getDashboards =
    let
        url_ = url ++ "dashboards"
        decode = Decode.at ["data"] (Decode.list decoder)
    in
    Http.get url_ decode

updateDashboard : Dashboard -> Http.Request Dashboard
updateDashboard a =
    let
        url_ = url ++ "dashboards/" ++ (toString a.id)
        decode = Decode.at ["data"] decoder
        body = Http.jsonBody (Encode.object [ ("dashboard", encode a) ])
        _ = Debug.log "UPDATE" a
    in
    API.put url_  body decode

createDashboard : Dashboard -> Http.Request Dashboard
createDashboard a =
    let
        url_ = url ++ "dashboards/"
        decode = Decode.at ["data"] decoder
        body = Http.jsonBody (Encode.object [ ("dashboard", encode a) ])
    in
    API.post url_ body decode


deleteDashboard: Int -> Http.Request ()
deleteDashboard a =
   let
       url_ = url ++ "dashboards/" ++ (toString a)
   in
   API.delete url_