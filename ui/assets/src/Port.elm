module Port exposing (Port, decoder, empty, encode, get, notState, off, on, toggle)

import API exposing (data, refBody)
import Http
import Id as Id exposing (Id)
import Json.Decode as Decode exposing (Decoder, andThen, bool, field, int, map6, nullable, string)
import Json.Decode.Pipeline as DP exposing (hardcoded, optional, required)
import Json.Encode as Encode


type Device
    = Nop


type alias Port =
    { id : Int
    , name : String
    , mode : String
    , state : Bool
    , type_ : String
    , timeout : Int
    , deviceId : Maybe Int

    --, device : Maybe Device
    }


empty : Port
empty =
    Port 0 "" "" False "" 0 Nothing


decoder : Decode.Decoder Port
decoder =
    Decode.succeed Port
        |> required "id" int
        |> required "name" string
        |> required "mode" string
        |> optional "state" bool False
        |> required "type" string
        |> required "timeout" int
        |> optional "device_id" (nullable int) Nothing


encode : Port -> Encode.Value
encode p =
    Encode.object
        [ ( "id", Encode.int p.id )
        , ( "name", Encode.string p.name )
        , ( "mode", Encode.string p.mode )
        , ( "state", Encode.bool p.state )
        , ( "type", Encode.string p.type_ )
        , ( "timeout", Encode.int p.timeout )

        --        , ("device_id", Encode.)
        ]



--FUNCTIONS


notState : Port -> Port
notState p =
    { p | state = not p.state }



-- CRUD ENDPOINTS


read : (Result Http.Error (List Port) -> msg) -> Cmd msg
read msg =
    API.get
        { path = "ports"
        , expect = Http.expectJson msg (Decode.list decoder)
        }


update : (Result Http.Error Port -> msg) -> Port -> Cmd msg
update msg a =
    API.put
        { path = "ports/" ++ String.fromInt a.id
        , body = Http.jsonBody (Encode.object [ ( "port", encode a ) ])
        , expect = Http.expectJson msg decoder
        }


create : (Result Http.Error Port -> msg) -> Port -> Cmd msg
create msg a =
    API.post
        { path = "ports"
        , body = Http.jsonBody (Encode.object [ ( "port", encode a ) ])
        , expect = Http.expectJson msg decoder
        }


delete : (Result Http.Error String -> msg) -> Port -> Cmd msg
delete msg a =
    API.delete ("ports/" ++ String.fromInt a.id) msg



-- ENDPOINTS


get : Int -> (Result Http.Error Port -> msg) -> Cmd msg
get id msg =
    API.get
        { path = "ports/cardView/" ++ String.fromInt id
        , expect = Http.expectJson msg decoder
        }


on : Maybe Int -> (Result Http.Error Port -> msg) -> Port -> Cmd msg
on sRef msg p =
    API.post
        { path = "ports/setOn" ++ String.fromInt p.id
        , expect = Http.expectJson msg decoder
        , body = refBody sRef []
        }


off : Maybe Int -> (Result Http.Error Port -> msg) -> Port -> Cmd msg
off sRef msg p =
    API.post
        { path = "ports/setOff" ++ String.fromInt p.id
        , expect = Http.expectJson msg decoder
        , body = refBody sRef []
        }


toggle : Maybe Int -> (Result Http.Error Port -> msg) -> Port -> Cmd msg
toggle sRef msg p =
    if p.state then
        off sRef msg p

    else
        on sRef msg p
