module Action exposing (Action, decoder, get, off, on, toggle)

import API exposing (data, refBody)
import Http
import Id as Id exposing (Id)
import Json.Decode as Decode exposing (Decoder, at, bool, field, int, list, nullable, string)
import Json.Decode.Pipeline as DP exposing (hardcoded, optional, required)
import Json.Encode as Encode
import Port as Port exposing (Port)


type alias Action =
    { id : Int
    , name : String
    , function : String
    , state : Bool
    , params : String
    , frequency : Int
    , startTime : Maybe String
    , endTime : Maybe String
    , portId : Maybe Int
    , port_ : Maybe Port
    }


empty : Action
empty =
    Action 0 "" "" False "{}" 1000 Nothing Nothing Nothing Nothing


decoder : Decode.Decoder Action
decoder =
    Decode.succeed Action
        |> required "id" int
        |> required "name" string
        |> required "function" string
        |> required "active" bool
        |> required "params" string
        |> required "frequency" int
        |> required "start_time" (nullable string)
        |> required "end_time" (nullable string)
        |> required "port_id" (nullable int)
        |> optional "port" (nullable Port.decoder) Nothing


encode : Action -> Encode.Value
encode a =
    let
        optional e v =
            v |> Maybe.map e >> Maybe.withDefault Encode.null
    in
    Encode.object
        [ ( "id", Encode.int a.id )
        , ( "name", Encode.string a.name )
        , ( "function", Encode.string a.function )
        , ( "active", Encode.bool a.state )
        , ( "params", Encode.string a.params )
        , ( "frequency", Encode.int a.frequency )
        , ( "start_time", optional Encode.string a.startTime )
        , ( "end_time", optional Encode.string a.endTime )
        , ( "port_id", optional Encode.int a.portId )
        ]



-- CRUD ENDPOINTS


read : (Result Http.Error (List Action) -> msg) -> Cmd msg
read msg =
    API.get
        { path = "actions"
        , expect = Http.expectJson msg (Decode.list decoder)
        }


update : (Result Http.Error Action -> msg) -> Action -> Cmd msg
update msg a =
    API.put
        { path = "actions/" ++ String.fromInt a.id
        , body = Http.jsonBody (Encode.object [ ( "action", encode a ) ])
        , expect = Http.expectJson msg decoder
        }


create : (Result Http.Error Action -> msg) -> Action -> Cmd msg
create msg a =
    API.post
        { path = "actions"
        , body = Http.jsonBody (Encode.object [ ( "action", encode a ) ])
        , expect = Http.expectJson msg decoder
        }


delete : (Result Http.Error String -> msg) -> Action -> Cmd msg
delete msg a =
    API.delete ("actions/" ++ String.fromInt a.id) msg



-- ENDPOINTS


get : Int -> (Result Http.Error Action -> msg) -> Cmd msg
get id msg =
    API.get
        { path = "actions/cardView/" ++ String.fromInt id
        , expect = Http.expectJson msg decoder
        }


on : Maybe Int -> (Result Http.Error Action -> msg) -> Action -> Cmd msg
on sRef msg a =
    API.post
        { path = "actions/setOn/" ++ String.fromInt a.id
        , expect = Http.expectJson msg decoder
        , body = refBody sRef []
        }


off : Maybe Int -> (Result Http.Error Action -> msg) -> Action -> Cmd msg
off sRef msg a =
    API.post
        { path = "actions/setOff/" ++ String.fromInt a.id
        , expect = Http.expectJson msg decoder
        , body = refBody sRef []
        }


toggle : Maybe Int -> (Result Http.Error Action -> msg) -> Action -> Cmd msg
toggle sRef msg a =
    if a.state then
        off sRef msg a

    else
        on sRef msg a
