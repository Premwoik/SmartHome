module Dashboard exposing (Content(..), Dashboard, DashboardShort, DashboardTabs, ShortContent, decoder, decoderContent, decoderShort, empty, getDashboard, getTabs, makeShortContent, updateContent, updateContentBlind, withAction, withDimmer, withLight, withPort, withSunblind, withTask)

import API exposing (data)
import Action as Action exposing (Action)
import Array exposing (Array)
import Dimmer as Dimmer exposing (Dimmer)
import Http
import Id as Id exposing (Id)
import Json.Decode as Decode exposing (Decoder, array, field, map, map3, map4, map7, string)
import Json.Decode.Pipeline as DP exposing (hardcoded, required)
import Json.Encode as Encode
import Light as Light exposing (Light)
import Port as Port exposing (Port)
import Sunblind as Sunblind exposing (Sunblind)
import TaskM as TaskM exposing (Task)


type alias DashboardTabs =
    List DashboardShort


type alias DashboardShort =
    { id : Int
    , number : Int
    , name : String
    }


type Content
    = Dimmer Dimmer
    | Light Light
    | Port Port
    | Action Action
    | Sunblind Sunblind
    | Task Task


type alias ShortContent =
    { type_ : String
    , id : Int
    }


type alias Dashboard =
    { id : Int
    , number : Int
    , name : String
    , title : String
    , description : String
    , content : List Content
    , shortContent : List ShortContent
    }


decoderShort : Decoder DashboardShort
decoderShort =
    Decode.map3 DashboardShort
        (field "id" Decode.int)
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
        , map Task TaskM.decoder
        ]


decoder : Decoder Dashboard
decoder =
    Decode.succeed Dashboard
        |> required "id" Decode.int
        |> required "order" Decode.int
        |> required "name" Decode.string
        |> required "title" Decode.string
        |> required "description" Decode.string
        |> required "content" (Decode.list decoderContent)
        |> hardcoded []


empty : Dashboard
empty =
    Dashboard -1 -1 "" "" "" [] []



--    API


makeShortContent : Dashboard -> Dashboard
makeShortContent d =
    let
        make : Content -> ShortContent
        make x =
            case x of
                Dimmer d_ ->
                    ShortContent "dimmer" d_.id

                Light l ->
                    ShortContent "light" l.id

                Action a ->
                    ShortContent "action" a.id

                Task t ->
                    ShortContent "task" t.id

                Port p ->
                    ShortContent "port" p.id

                Sunblind s ->
                    ShortContent "sunblind" s.id
    in
    { d | shortContent = d.content |> List.map make }


withPort : (Port -> Port) -> Content -> Content
withPort func con =
    case con of
        Port p ->
            Port <| func p

        _ ->
            con


withDimmer : (Dimmer -> Dimmer) -> Content -> Content
withDimmer func con =
    case con of
        Dimmer d ->
            Dimmer <| func d

        _ ->
            con


withSunblind : (Sunblind -> Sunblind) -> Content -> Content
withSunblind func con =
    case con of
        Sunblind s ->
            Sunblind <| func s

        _ ->
            con


withLight : (Light -> Light) -> Content -> Content
withLight func con =
    case con of
        Light l ->
            Light <| func l

        _ ->
            con


withTask : (Task -> Task) -> Content -> Content
withTask func con =
    case con of
        Task t ->
            Task <| func t

        _ ->
            con


withAction : (Action -> Action) -> Content -> Content
withAction func con =
    case con of
        Action a ->
            Action <| func a

        _ ->
            con


updateContent :
    { cnt | id : Int }
    -> (({ cnt | id : Int } -> { cnt | id : Int }) -> Content -> Content)
    -> List Content
    -> List Content
updateContent new with l =
    let
        replace old =
            if new.id == old.id then
                new

            else
                old
    in
    List.map (with <| replace) l


updateContentBlind : Content -> List Content -> List Content
updateContentBlind c l =
    case c of
        Dimmer v ->
            updateContent v withDimmer l

        Light v ->
            updateContent v withLight l

        Port v ->
            updateContent v withPort l

        Action v ->
            updateContent v withAction l

        Sunblind v ->
            updateContent v withSunblind l

        Task v ->
            updateContent v withTask l


getTabs : (Result Http.Error (List DashboardShort) -> msg) -> Cmd msg
getTabs msg =
    API.get
        { path = "dashboards/view/short"
        , expect = Http.expectJson msg (Decode.list decoderShort)
        }


getDashboard : Int -> (Result Http.Error Dashboard -> msg) -> Cmd msg
getDashboard id msg =
    API.get
        { path = "dashboards/view/" ++ String.fromInt id
        , expect = Http.expectJson msg decoder
        }


decodeUpdateNoti : Encode.Value -> Result Decode.Error (List ShortContent)
decodeUpdateNoti raw =
    let
        objDecoder =
            Decode.map2 ShortContent (field "type" Decode.string) (field "id" Decode.int)

        decoder_ =
            Decode.list objDecoder
    in
    Decode.decodeValue decoder_ raw
