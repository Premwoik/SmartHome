module Page.Management exposing (Model, Msg, mdl, update, view)

import Html exposing (..)


type alias Loaded x =
    Maybe x


type alias Model =
    { content : Loaded String }


mdl : Model
mdl =
    { content = Nothing }


type Msg
    = Nop


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        _ ->
            ( model, Cmd.none )


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "Management" ] ]
