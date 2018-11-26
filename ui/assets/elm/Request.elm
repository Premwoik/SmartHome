module Request exposing (..)

import Http exposing (Body, Request, expectJson, request)
import Json.Decode as Decode
import Task

url = "http://0.0.0.0:4000/api/"
--url = "http://192.168.2.119:4000/api/"
--url = "http://192.168.2.100/api/"


send : (Result Http.Error a -> msg) -> Http.Request a -> Cmd msg
send msg request =
    Http.send msg request


send2 : (Result Http.Error (x,y) -> msg) -> Http.Request x -> Http.Request y-> Cmd msg
send2 msg request1 request2 =
    Task.attempt msg
        <| Task.map2 (\resp1 resp2 -> (resp1, resp2))
            (Http.toTask request1) (Http.toTask request2)


post : String -> Body -> Decode.Decoder a -> Request a
post url body decoder =
    request
        { method = "POST"
                , headers = []
                , url = url
                , body = body
                , expect = expectJson decoder
                , timeout = Nothing
                , withCredentials = False
                }


data : Decode.Decoder a -> Decode.Decoder a
data d =
    Decode.at ["data"] d

put : String -> Body -> Decode.Decoder a -> Request a
put url body decoder =
  request
    { method = "PUT"
    , headers = []
    , url = url
    , body = body
    , expect = expectJson decoder
    , timeout = Nothing
    , withCredentials = False
    }

delete : String -> Request ()
delete url =
  request
    { method = "DELETE"
    , headers = []
    , url = url
    , body = Http.emptyBody
    , expect = Http.expectStringResponse << always <| Ok ()
    , timeout = Nothing
    , withCredentials = False
    }
