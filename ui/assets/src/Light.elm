module Light exposing (Light, decoder, get, notState, off, on, toggle)

import API exposing (data, refBody)
import Http
import Id as Id
import Json.Decode as Decode exposing (Decoder, andThen, bool, field, int, nullable, string)
import Json.Encode as Encode
import Port exposing (Port)


type alias Light =
    { id : Int
    , port_ : Port
    , dimmer_id : Maybe Int
    }



--DECODER


decoder : Decoder Light
decoder =
    Decode.map3 Light
        (field "id" int)
        (field "port" Port.decoder)
        (field "dimmer_id" (nullable int))


encode : Light -> Encode.Value
encode a =
    let
        optional e v =
            v |> Maybe.map e >> Maybe.withDefault Encode.null
    in
    Encode.object
        [ ( "id", Encode.int a.id )
        , ( "port", Port.encode a.port_ )
        , ( "dimmer_id", optional Encode.int a.dimmer_id )
        ]



-- FUNCTIONS


notState : Light -> Light
notState l =
    { l | port_ = Port.notState l.port_ }



-- CRUD ENDPOINTS


read : (Result Http.Error (List Light) -> msg) -> Cmd msg
read msg =
    API.get
        { path = "actions"
        , expect = Http.expectJson msg (Decode.list decoder)
        }


update : (Result Http.Error Light -> msg) -> Light -> Cmd msg
update msg l =
    API.put
        { path = "lights/" ++ String.fromInt l.id
        , body = Http.jsonBody (Encode.object [ ( "light", encode l ) ])
        , expect = Http.expectJson msg decoder
        }


create : (Result Http.Error Light -> msg) -> Light -> Cmd msg
create msg l =
    API.post
        { path = "lights"
        , body = Http.jsonBody (Encode.object [ ( "light", encode l ) ])
        , expect = Http.expectJson msg decoder
        }


delete : (Result Http.Error String -> msg) -> Light -> Cmd msg
delete msg l =
    API.delete ("lights/" ++ String.fromInt l.id) msg



--ENDPOINTS


get : Int -> (Result Http.Error Light -> msg) -> Cmd msg
get id msg =
    API.get
        { path = "lights/cardView/" ++ String.fromInt id
        , expect = Http.expectJson msg decoder
        }


on : Maybe Int -> (Result Http.Error Light -> msg) -> Light -> Cmd msg
on sRef msg l =
    API.post
        { path = "lights/setOn/" ++ String.fromInt l.id
        , expect = Http.expectJson msg decoder
        , body = refBody sRef []
        }


off : Maybe Int -> (Result Http.Error Light -> msg) -> Light -> Cmd msg
off sRef msg l =
    API.post
        { path = "lights/setOff/" ++ String.fromInt l.id
        , expect = Http.expectJson msg decoder
        , body = refBody sRef []
        }


toggle : Maybe Int -> (Result Http.Error Light -> msg) -> Light -> Cmd msg
toggle sRef msg l =
    if l.port_.state then
        off sRef msg l

    else
        on sRef msg l
