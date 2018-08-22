module Data.Sunblind exposing (..)--(set, isOpen, setList, State, Sunblind, decoder, setClose, setOpen, toggle)
import Json.Decode as Decode exposing (string, map, field, Decoder, bool, map3)
import Data.Id as Id

type alias Sunblind =
    {
        id : Id.Id,
        name : String,
        state : State
    }


decoder : Decoder Sunblind
decoder =
    map3 Sunblind (field "id" Id.decoder)
            (field "name" string)
            (field "state" stateDecoder)

-- STATE

type State =
    Open | Close

notState : State -> State
notState state =
    case state of
        Open -> Close
        Close -> Open

stateDecoder : Decoder State
stateDecoder =
    bool
        |> Decode.andThen(\b ->
            case b of
                True -> Decode.succeed Open
                False -> Decode.succeed Close
            )

-- FUNCTIONS
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

toggleList : List Sunblind -> List Sunblind
toggleList list =
    let
        state = List.head list |> (\x ->
            case x of
                Just light -> light.state
                Nothing -> Close
            ) |> notState
    in
    setList list state

toggle : Sunblind -> Sunblind
toggle s=
    set s (notState s.state)


setList : List Sunblind -> State -> List Sunblind
setList s state =
    List.map (\x -> {x | state = state}) s

set : Sunblind -> State-> Sunblind
set s state =
    {s | state = state}

