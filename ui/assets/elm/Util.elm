module Util exposing ((=>), appendErrors, onClickStopPropagation, pair, viewIf, replaceListElem)

import Html exposing (Attribute, Html)
import Html.Events exposing (defaultOptions, onWithOptions)
import Json.Decode as Decode


(=>) : a -> b -> ( a, b )
(=>) =
    (,)


--{-| infixl 0 means the (=>) operator has the same precedence as (<|) and (|>),
--meaning you can use it at the end of a pipeline and have the precedence work out.
---}
----infixl 0 =>
--
--
--{-| Useful when building up a Cmd via a pipeline, and then pairing it with
--a model at the end.
--    session.user
--        |> User.Request.foo
--        |> Task.attempt Foo
--        |> pair { model | something = blah }
---}
pair : a -> b -> ( a, b )
pair first second =
    first => second


viewIf : Bool -> Html msg -> Html msg
viewIf condition content =
    if condition then
        content
    else
        Html.text ""


onClickStopPropagation : msg -> Attribute msg
onClickStopPropagation msg =
    onWithOptions "click"
        { defaultOptions | stopPropagation = True }
        (Decode.succeed msg)


appendErrors : { model | errors : List error } -> List error -> { model | errors : List error }
appendErrors model errors =
    { model | errors = model.errors ++ errors }


--replaceListElem : List record -> record -> List record
replaceListElem list record =
    let
        f = \x -> if x.id == record.id then record else x
    in
    List.map f list

--updateListElem : List reocrd -> (record -> record) -> List record
updateListElem list fn =
    List.map fn list
