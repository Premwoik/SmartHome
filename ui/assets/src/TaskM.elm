module TaskM exposing (Status(..), Task, decoder, get, off, on, statusDecoder, toggle)

import API exposing (Request, data, refBody)
import Http
import Json.Decode as Decode
import Json.Decode.Pipeline as DP exposing (required)
import Json.Encode as Encode


type alias Task =
    { id : Int
    , name : String
    , type_ : String
    , status : Status
    }


type Status
    = Waiting
    | Inactive
    | Running



--DECODER


statusDecoder : Decode.Decoder Status
statusDecoder =
    Decode.string
        |> Decode.andThen
            (\b ->
                case b of
                    "inactive" ->
                        Decode.succeed Inactive

                    "running" ->
                        Decode.succeed Running

                    "waiting" ->
                        Decode.succeed Waiting

                    _ ->
                        Decode.fail ("Wrong state value. It can only be one of 'inactive', 'running' or 'waiting', when is: " ++ b)
            )


decoder : Decode.Decoder Task
decoder =
    Decode.succeed Task
        |> required "id" Decode.int
        |> required "name" Decode.string
        |> required "type" Decode.string
        |> required "status" statusDecoder



--ENDOPOINTS


get : Int -> (Result Http.Error Task -> msg) -> Cmd msg
get id msg =
    API.get
        { path = "tasks/cardView/" ++ String.fromInt id
        , expect = expect msg
        }


on : Maybe Int -> (Result Http.Error Task -> msg) -> Task -> Cmd msg
on sRef msg t =
    API.post
        { path = "tasks/setOn/" ++ String.fromInt t.id
        , expect = expect msg
        , body = refBody sRef []
        }


off : Maybe Int -> (Result Http.Error Task -> msg) -> Task -> Cmd msg
off sRef msg t =
    API.post
        { path = "tasks/setOff/" ++ String.fromInt t.id
        , expect = expect msg
        , body = refBody sRef []
        }


toggle : Maybe Int -> (Result Http.Error Task -> msg) -> Task -> Cmd msg
toggle sRef msg t =
    if t.status /= Inactive then
        off sRef msg t

    else
        on sRef msg t



--PRIVATES


expect : (Result Http.Error Task -> msg) -> Http.Expect msg
expect msg =
    Http.expectJson msg decoder
