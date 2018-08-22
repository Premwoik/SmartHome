module Data.Light exposing (..) --(setOnList, setOffList, isOn, Light, setOff, setOn, toggle, decoder)
import Data.Id as Id
import Json.Decode as Decode exposing (field, bool, string, map3, Decoder)
type alias Light =
    {
        id : Id.Id,
        state : State,
        name : String
    }

decoder : Decoder Light
decoder =
    map3 Light (field "id" Id.decoder)
        (field "state" stateDecoder)
        (field "name" string)

-- STATE

type State =
    On | Off

notState : State -> State
notState state =
    case state of
        On -> Off
        Off -> On

stateDecoder : Decoder State
stateDecoder =
    bool
        |> Decode.andThen (\state ->
            case state of
                True -> Decode.succeed On
                False -> Decode.succeed Off
            )

-- FUNCTIONS

getId : Light -> Int
getId sun =
    Id.toInt sun.id

isOn : Light -> Bool
isOn light =
    light.state == On

setOnList : List Light -> List Light
setOnList lights =
    setList lights On

setOn : Light -> Light
setOn light =
    set light On


setOffList : List Light -> List Light
setOffList lights =
    setList lights Off

setOff : Light -> Light
setOff light =
    set light On

toggleList : List Light -> List Light
toggleList lights =
    let
        state = List.head lights |> (\x ->
            case x of
                Just light -> light.state
                Nothing -> Off
            ) |> notState
    in
    setList lights state

toggle : Light -> Light
toggle light =
    set light (notState light.state)


setList : List Light -> State -> List Light
setList lights state =
    List.map (\x -> {x | state = state}) lights

set : Light -> State-> Light
set light state =
    {light | state = state}

