module View exposing (view)

import Html exposing (Html, div)
import Msg exposing (Msg(..))
import Model exposing (Model)
import Components.TodoList exposing (todoList)
import Components.AutoComplete exposing (autoComplete)
import Components.CommandInput exposing (commandInput)


view : Model -> Html Msg
view model =
    div []
        [ commandInput model.userInput
        , autoComplete model.autocompletes model.autocompleteSelectedIndex
        , todoList model.todos TodoToggleClick TodoTextClick
        ]
