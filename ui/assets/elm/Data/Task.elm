module Data.Task exposing (..)


import Http
import Json.Decode.Pipeline as DP exposing (required, decode)
import Json.Decode as Decode
import Json.Encode as Encode
import Request exposing (data, refBody)

type alias Task =
    { id : Int
    , name : String
    , type_ : String
    , status : Status
    , task: String
    }


type Status
    = Waiting
    | Inactive
    | Running


statusDecoder : Decode.Decoder Status
statusDecoder =
    Decode.string
        |> Decode.andThen (\b ->
            case b of
                "inactive" -> Decode.succeed Inactive
                "running" -> Decode.succeed Running
                "waiting" -> Decode.succeed Waiting
                _ -> Decode.fail ("Wrong state value. It can only be one of 'inactive', 'running' or 'waiting', when is: " ++ b)
            )

decoder : Decode.Decoder Task
decoder =
    decode Task
    |> required "id" Decode.int
    |> required "name" Decode.string
    |> required "type" Decode.string
    |> required "status" statusDecoder
    |> required "task" Decode.string

setOn : Maybe Int -> Task -> Http.Request Task
setOn sRef t =
    let
        url_ = Request.url ++ "tasks/setOn/" ++ (toString t.id)

    in
    Http.post url_ (refBody sRef []) (data decoder)

setOff : Maybe Int -> Task -> Http.Request Task
setOff sRef t =
    let
        url_ = Request.url ++ "tasks/setOff/" ++ (toString t.id)

    in
    Http.post url_ (refBody sRef []) (data decoder)


toggle : Maybe Int -> Task ->  Http.Request Task
toggle sRef t=
   if t.status /= Inactive then setOff sRef t else setOn sRef t


getView : Int -> Http.Request Task
getView id =
  let
      url_ = Request.url ++ "tasks/cardView/" ++ (toString id)
  in
  Http.get url_ (data decoder)
