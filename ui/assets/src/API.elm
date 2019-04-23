module API exposing (Request, data, delete, get, post, put, refBody, setStateEndpoint)

import Http
import Json.Decode as Decode
import Json.Encode as Encode



--url =
--"http://0.0.0.0:4000/api/"
--url = "http://192.168.2.119:4000/api/"


url =
    "http://192.168.2.100/api/"


type alias Request msg =
    { path : String
    , expect : Http.Expect msg
    , body : Http.Body
    }



--METHODS


get : { path : String, expect : Http.Expect msg } -> Cmd msg
get request =
    Http.get
        { url = makeUrl request
        , expect = request.expect
        }


post : Request msg -> Cmd msg
post request =
    Http.request
        { method = "POST"
        , headers = []
        , url = makeUrl request
        , body = request.body
        , expect = request.expect
        , timeout = Nothing
        , tracker = Nothing
        }


put : Request msg -> Cmd msg
put req =
    Http.request
        { method = "PUT"
        , headers = []
        , url = makeUrl req
        , body = req.body
        , expect = req.expect
        , timeout = Nothing
        , tracker = Nothing
        }


delete : String -> (Result Http.Error String -> msg) -> Cmd msg
delete path msg =
    Http.request
        { method = "DELETE"
        , headers = []
        , url = url ++ path
        , body = Http.emptyBody
        , expect = Http.expectString msg
        , timeout = Nothing
        , tracker = Nothing
        }



--HELPERS


setStateEndpoint :
    { sRef : Maybe Int
    , path : String
    , msg : Result Http.Error type_ -> msg
    , decoder : Decode.Decoder type_
    }
    -> Cmd msg
setStateEndpoint { sRef, path, msg, decoder } =
    post
        { path = path
        , expect = Http.expectJson msg decoder
        , body = refBody sRef []
        }


refBody : Maybe Int -> List ( String, Encode.Value ) -> Http.Body
refBody s v =
    case s of
        Just val ->
            Http.jsonBody <| Encode.object (( "joinRef", Encode.int val ) :: v)

        Nothing ->
            Http.emptyBody


data : Decode.Decoder a -> Decode.Decoder a
data d =
    Decode.at [ "data" ] d



--PRIVATES


makeUrl : { b | path : String } -> String
makeUrl { path } =
    url ++ path
