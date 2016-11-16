module Components.TodoList exposing (..)

import Html exposing (..)
import Html.Attributes exposing (class)
import Types exposing (Todo, Id)
import Components.Todo exposing (todo)


todoList : List Todo -> (Id -> a) -> (Todo -> a) -> Html a
todoList todos todoToggleClick todoTextClick =
    ul
        [ class "list-group cm-todos" ]
    <|
        List.indexedMap (todo todoToggleClick todoTextClick) todos
