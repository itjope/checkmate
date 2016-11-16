module Components.TodoList exposing (todoList)

import Html exposing (Html, ul)
import Html.Attributes exposing (class)
import Types exposing (Todo)
import Components.Todo exposing (todo)
import Msg exposing (Msg)


todoList : List Todo -> Html Msg
todoList todos =
    ul
        [ class "list-group cm-todos" ]
    <|
        List.indexedMap todo todos
