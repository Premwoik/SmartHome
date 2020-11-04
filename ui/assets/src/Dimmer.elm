module Dimmer exposing (Dimmer, Direction(..), create, decoder, delete, directionDecoder, encode, get, isOn, off, on, read, setFill, setLightOff, setLightOn, toggle, toggleLight, update)

import API exposing (data, refBody, setStateEndpoint)
import Http
import Id as Id
import Json.Decode as Decode exposing (Decoder, bool, field, float, int, list, map5, nullable, string)
import Json.Decode.Pipeline as DP exposing (hardcoded, optional, required)
import Json.Encode as Encode
import Light exposing (Light)
import Port exposing (Port)


type alias Dimmer =
    { id : Int
    , fill : Int
    , direction : Direction
    , fullTime : Int
    , lights : Maybe (List Light)
    , port_ : Port
    }


type Direction
    = Up
    | Down


decoder : Decoder Dimmer
decoder =
    Decode.succeed Dimmer
        |> required "id" int
        |> required "fill" int
        |> required "direction" directionDecoder
        |> required "full_time" int
        |> optional "lights" (nullable (list Light.decoder)) Nothing
        |> required "port" Port.decoder


encode : Dimmer -> Encode.Value
encode d =
    --TODO should be encoded whole object
    Encode.object
        [ ( "id", Encode.int d.id )
        , ( "fill", Encode.int d.fill )
        , ( "direction", directionEncoder d.direction )
        , ( "time", Encode.int d.fullTime )
        , ( "port", Port.encode d.port_ )
        ]


encodeMaybe : Maybe x -> (x -> Encode.Value) -> Encode.Value
encodeMaybe x encoder =
    case x of
        Just val ->
            encoder val

        Nothing ->
            Encode.null


directionDecoder : Decoder Direction
directionDecoder =
    int
        |> Decode.andThen
            (\dir ->
                case dir of
                    1 ->
                        Decode.succeed Up

                    _ ->
                        Decode.succeed Down
            )


directionEncoder : Direction -> Encode.Value
directionEncoder d =
    case d of
        Up ->
            Encode.int 1

        Down ->
            Encode.int -1



-- FUNCTIONS


isOn : Dimmer -> Bool
isOn { lights, fill } =
    let
        _ =
            Debug.log "L" (Debug.toString lights)

        _ =
            Debug.log "Fill" (Debug.toString fill)
    in
    case lights of
        Just l ->
            List.any (.port_ >> .state) l && fill > 0

        Nothing ->
            fill > 0



-- CRUD ENDPOINTS


read : (Result Http.Error (List Dimmer) -> msg) -> Cmd msg
read msg =
    API.get
        { path = "dimmers"
        , expect = Http.expectJson msg (Decode.list decoder)
        }


update : (Result Http.Error Dimmer -> msg) -> Dimmer -> Cmd msg
update msg d =
    API.put
        { path = "dimmers/" ++ String.fromInt d.id
        , body = Http.jsonBody (Encode.object [ ( "dimmer", encode d ) ])
        , expect = Http.expectJson msg decoder
        }


create : (Result Http.Error Dimmer -> msg) -> Dimmer -> Cmd msg
create msg d =
    API.post
        { path = "actions"
        , body = Http.jsonBody (Encode.object [ ( "dimmer", encode d ) ])
        , expect = Http.expectJson msg decoder
        }


delete : (Result Http.Error String -> msg) -> Dimmer -> Cmd msg
delete msg d =
    API.delete ("actions/" ++ String.fromInt d.id) msg



-- ENDPOINTS


setFill : Float -> Maybe Int -> (Result Http.Error Dimmer -> msg) -> Dimmer -> Cmd msg
setFill fill sRef msg d =
    API.post
        { path = "dimmers/setBrightness"
        , expect = Http.expectJson msg decoder
        , body =
            refBody sRef
                [ ( "id", Encode.int d.id )
                , ( "fill", Encode.int (round fill) )
                ]
        }


on : Maybe Int -> (Result Http.Error Dimmer -> msg) -> Dimmer -> Cmd msg
on sRef msg d =
    API.setStateEndpoint
        { sRef = sRef
        , path = "dimmers/setOn/" ++ String.fromInt d.id
        , msg = msg
        , decoder = decoder
        }


off : Maybe Int -> (Result Http.Error Dimmer -> msg) -> Dimmer -> Cmd msg
off sRef msg d =
    API.setStateEndpoint
        { sRef = sRef
        , path = "dimmers/setOff/" ++ String.fromInt d.id
        , msg = msg
        , decoder = decoder
        }


toggle : Maybe Int -> (Result Http.Error Dimmer -> msg) -> Dimmer -> Cmd msg
toggle sRef msg d =
    if isOn d then
        off sRef msg d

    else
        on sRef msg d


setLightOn : Maybe Int -> (Result Http.Error Dimmer -> msg) -> Light -> Cmd msg
setLightOn sRef msg l =
    API.setStateEndpoint
        { sRef = sRef
        , path = "dimmers/setLightOn/" ++ String.fromInt l.id
        , msg = msg
        , decoder = decoder
        }


setLightOff : Maybe Int -> (Result Http.Error Dimmer -> msg) -> Light -> Cmd msg
setLightOff sRef msg l =
    API.setStateEndpoint
        { sRef = sRef
        , path = "dimmers/setLightOff/" ++ String.fromInt l.id
        , msg = msg
        , decoder = decoder
        }


toggleLight : Maybe Int -> (Result Http.Error Dimmer -> msg) -> Light -> Cmd msg
toggleLight sRef msg l =
    if l.port_.state then
        setLightOff sRef msg l

    else
        setLightOn sRef msg l


get : Int -> (Result Http.Error Dimmer -> msg) -> Cmd msg
get id msg =
    API.get
        { path = "dimmers/cardView" ++ String.fromInt id
        , expect = Http.expectJson msg decoder
        }
