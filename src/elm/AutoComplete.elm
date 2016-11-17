module AutoComplete exposing (autoComplete)

import Html exposing (Html, ul, li, text)
import Html.Attributes exposing (class)
import Todo exposing (Todo)


getClass : Bool -> String
getClass isSelected =
    if isSelected then
        "list-group-item selected"
    else
        "list-group-item"


autoComplete : List Todo -> Int -> Html a
autoComplete todos selectedIndex =
    ul
        [ class "list-group cm-autocomplete" ]
    <|
        List.indexedMap (\index todo -> li [ class (getClass (selectedIndex == index)) ] [ text todo.text ]) todos
