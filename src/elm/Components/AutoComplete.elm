module Components.AutoComplete exposing (autoComplete)

import Html exposing (..)
import Html.Attributes exposing (class)
import Types exposing (Todo)


getClass : Bool -> String
getClass isSelected =
    if isSelected then
        "list-group-item selected"
    else
        "list-group-item"


autoComplete : List Todo -> Int -> Html a
autoComplete todos selectedIndex =
    ul
        [ class "list-group" ]
    <|
        List.indexedMap (\index todo -> li [ class (getClass (selectedIndex == index)) ] [ text todo.text ]) todos
