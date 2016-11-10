module Components.TodoList exposing (..)

import Html exposing (..)
import Html.Attributes exposing (class)
import Components.Todo exposing (Todo, todo)


todoList : List Todo -> a -> Html a
todoList model msg =
    ul
        [ class "list-group" ]
    <|
        List.map (todo msg) model
