module Data.Sunblind exposing (..)--(set, isOpen, setList, State, Sunblind, decoder, setClose, setOpen, toggle)
import Json.Decode as Decode exposing (string, map, field, Decoder, bool, map4, andThen)
import Data.Id as Id

import Http
import Request
import Json.Encode as Encode


type alias Sunblind =
    {
        id : Id.Id,
        name : String,
        state : State,
        order : Int,
        sunblind_ : String
    }


decoder : Decoder Sunblind
decoder =
    Decode.map5 Sunblind (field "id" Id.decoder)
            (field "name" string)
            (field "state" stateDecoder)
            (field "order" Decode.int)
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


getId : Sunblind -> Int
getId sun =
    Id.toInt sun.id


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



setOn : Sunblind -> Http.Request Sunblind
setOn s =
    let
        url_ = Request.url ++ "sunblinds/setOn"
        data = Encode.object
            [ ("id", Encode.int (Id.toInt s.id))
            ]
    in
    Http.post url_ (Http.jsonBody data) decoder

setOff : Sunblind -> Http.Request Sunblind
setOff s =
    let
        url_ = Request.url ++ "sunblinds/setOff"
        data = Encode.object
            [ ("id", Encode.int (Id.toInt s.id))
            ]
    in
    Http.post url_ (Http.jsonBody data) decoder

changePosition : Sunblind -> Http.Request Sunblind
changePosition s =
     let
        url_ = Request.url ++ "sunblinds/changePosition"
        data = Encode.object
            [ ("id", Encode.int (Id.toInt s.id))
            ]
     in
     Http.post url_ (Http.jsonBody data) decoder


click : Sunblind -> Http.Request Sunblind
click  s =
      let
         url_ = Request.url ++ "sunblinds/click"
         data = Encode.object
             [ ("id", Encode.int (Id.toInt s.id))
             ]
      in
     Request.post url_ (Http.jsonBody data) decoder

toggle : Sunblind -> Http.Request Sunblind
toggle s =
    case s.state of
       Open -> setOff s
       Close -> setOn s
       _ -> changePosition s

setState : Sunblind -> State -> Http.Request Sunblind
setState s state =
    let
        url_ = Request.url ++ "sunblinds/calibrate"
        data = Encode.object
            [ ("id", Encode.int (Id.toInt s.id))
            , ("state", stateEncoder state)
            ]
    in
    Http.post url_ (Http.jsonBody data) decoder


toggleManual : Sunblind -> Http.Request Sunblind
toggleManual s =
    if s.state == Position then setState s Open else setState s Position