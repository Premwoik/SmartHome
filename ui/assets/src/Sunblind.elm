module Sunblind exposing (State(..), Sunblind, click, decoder, get, isManual, isOpen, manual, nextState, set, setClose, setCloseList, setList, setOpen, setOpenList, setState, stateDecoder, stateEncoder, toggleManual)

import API exposing (data, refBody)
import Http
import Json.Decode as Decode exposing (Decoder, andThen, bool, field, map, map4, string)
import Json.Encode as Encode


type alias Sunblind =
    { id : Int
    , name : String
    , state : State
    }


decoder : Decoder Sunblind
decoder =
    Decode.map3 Sunblind
        (field "id" Decode.int)
        (field "name" string)
        (field "state" stateDecoder)



-- STATE


type State
    = Open
    | Close
    | InMove
    | Position


stateDecoder : Decoder State
stateDecoder =
    string
        |> Decode.andThen
            (\b ->
                case b of
                    "open" ->
                        Decode.succeed Open

                    "close" ->
                        Decode.succeed Close

                    "in_move" ->
                        Decode.succeed InMove

                    "position" ->
                        Decode.succeed Position

                    _ ->
                        Decode.fail "Wrong state value"
            )


stateEncoder : State -> Encode.Value
stateEncoder s =
    let
        toString =
            case s of
                Open ->
                    "open"

                Close ->
                    "close"

                InMove ->
                    "in_move"

                Position ->
                    "position"
    in
    Encode.string toString



-- FUNCTIONS


nextState : Sunblind -> Sunblind
nextState s =
    case s.state of
        Open ->
            { s | state = Close }

        Close ->
            { s | state = Open }

        _ ->
            s


manual : Sunblind -> Sunblind
manual s =
    case s.state of
        Position ->
            { s | state = Open }

        _ ->
            { s | state = Position }


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
    List.map (\x -> { x | state = state }) s


set : Sunblind -> State -> Sunblind
set s state =
    { s | state = state }



-- ENDPOINTS


click : Maybe Int -> (Result Http.Error Sunblind -> msg) -> Sunblind -> Cmd msg
click sRef msg s =
    API.post
        { path = "sunblinds/click/" ++ String.fromInt s.id
        , expect = Http.expectJson msg decoder
        , body = refBody sRef []
        }


setState : Maybe Int -> (Result Http.Error Sunblind -> msg) -> Sunblind -> Cmd msg
setState sRef msg s =
    API.post
        { path = "sunblinds/calibrate"
        , expect = Http.expectJson msg decoder
        , body =
            refBody sRef
                [ ( "id", Encode.int s.id )
                , ( "state", stateEncoder s.state )
                ]
        }


toggleManual : Maybe Int -> (Result Http.Error Sunblind -> msg) -> Sunblind -> Cmd msg
toggleManual sRef msg s =
    if s.state == Position then
        setState sRef msg { s | state = Open }

    else
        setState sRef msg { s | state = Position }


get : Int -> (Result Http.Error Sunblind -> msg) -> Cmd msg
get id msg =
    API.get
        { path = "sunblinds/cardView/" ++ String.fromInt id
        , expect = Http.expectJson msg decoder
        }
