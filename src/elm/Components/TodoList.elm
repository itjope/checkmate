module Components.TodoList exposing (..)

import Html exposing (..)
import Html.Attributes exposing (class)
import Components.Todo exposing (Todo, todo)


todoList : List Todo -> (String -> a) -> Html a
todoList todos msg =
    ul
        [ class "list-group" ]
    <|
        List.indexedMap (todo msg) todos
