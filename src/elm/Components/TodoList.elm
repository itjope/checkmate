module Components.TodoList exposing (..)

import Html exposing (..)
import Html.Attributes exposing (class)
import Components.Todo exposing (Todo, Id, todo)


todoList : List Todo -> (Id -> a) -> (Todo -> a) -> Html a
todoList todos todoToggleClick todoTextClick =
    ul
        [ class "list-group" ]
    <|
        List.indexedMap (todo todoToggleClick todoTextClick) todos
