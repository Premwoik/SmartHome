module Request exposing (..)

import Http
import Task

--url = "http://0.0.0.0:4000/api/"
url = "http://192.168.2.119:4000/api/"
--url = "http://192.168.2.100/api/"


send : (Result Http.Error a -> msg) -> Http.Request a -> Cmd msg
send msg request =
    Http.send msg request


send2 : (Result Http.Error (x,y) -> msg) -> Http.Request x -> Http.Request y-> Cmd msg
send2 msg request1 request2 =
    Task.attempt msg
        <| Task.map2 (\resp1 resp2 -> (resp1, resp2))
            (Http.toTask request1) (Http.toTask request2)

