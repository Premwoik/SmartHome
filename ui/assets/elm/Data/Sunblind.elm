module Data.Sunblind exposing (..)--(set, isOpen, setList, State, Sunblind, decoder, setClose, setOpen, toggle)
import Json.Decode as Decode exposing (string, map, field, Decoder, bool, map4, andThen)
import Data.Id as Id

import Http
import Request exposing (data, refBody)
import Json.Encode as Encode


type alias Sunblind =
    {
        id : Int,
        name : String,
        state : State,
        sunblind_ : String
    }


decoder : Decoder Sunblind
decoder =
    Decode.map4 Sunblind (field "id" Decode.int)
            (field "name" string)
            (field "state" stateDecoder)
            (field "sunblind"  string)


-- STATE

type State
    = Open
    | Close
    | InMove
    | Position


stateDecoder : Decoder State
stateDecoder =
    string
        |> Decode.andThen(\b ->
            case b of
                "open" -> Decode.succeed Open
                "close" -> Decode.succeed Close
                "in_move" -> Decode.succeed InMove
                "position" -> Decode.succeed Position
                _ -> Decode.fail "Wrong state value"
            )

stateEncoder : State -> Encode.Value
stateEncoder s =
    let
        toString =
            case s of
                Open -> "open"
                Close -> "close"
                InMove -> "in_move"
                Position -> "position"
    in
    Encode.string toString

-- FUNCTIONS

isManual : Sunblind -> Bool
isManual s =
    s.state == Position


isOpen : Sunblind -> Bool
isOpen sun =
    sun.state == Open

setOpenList : List Sunblind -> List Sunblind
setOpenList list =
    setList list Open

setOpen : Sunblind -> Sunblind
setOpen s =
    set s Open

setCloseList : List Sunblind -> List Sunblind
setCloseList l =
    setList l Close

setClose : Sunblind -> Sunblind
setClose s =
    set s Close




setList : List Sunblind -> State -> List Sunblind
setList s state =
    List.map (\x -> {x | state = state}) s

set : Sunblind -> State-> Sunblind
set s state =
    {s | state = state}


-- API


click : Maybe Int-> Sunblind -> Http.Request Sunblind
click sRef s =
      let
         url_ = Request.url ++ "sunblinds/click/" ++ toString(s.id)
      in
     Request.post url_ (refBody sRef []) (data decoder)


setState : Maybe Int -> Sunblind -> State -> Http.Request Sunblind
setState sRef s state =
    let
        url_ = Request.url ++ "sunblinds/calibrate"
        data_ =
            [ ("id", Encode.int s.id)
            , ("state", stateEncoder state)
            ]
    in
    Http.post url_ (refBody sRef data_) (data decoder)


toggleManual : Maybe Int -> Sunblind -> Http.Request Sunblind
toggleManual sRef s=
    if s.state == Position then setState sRef s Open else setState sRef s Position



getView : Int -> Http.Request Sunblind
getView id =
  let
      url_ = Request.url ++ "sunblinds/cardView/" ++ (toString id)
  in
  Http.get url_ (data decoder)