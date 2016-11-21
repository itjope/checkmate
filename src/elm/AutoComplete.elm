module AutoComplete exposing (autoComplete, AutocompleteItem)

import Html exposing (Html, ul, li, text)
import Html.Attributes exposing (class)


type alias AutocompleteItem =
    { key : String
    , text : String
    }


getClass : Bool -> String
getClass isSelected =
    if isSelected then
        "list-group-item active"
    else
        "list-group-item"


autoComplete : List AutocompleteItem -> Int -> Html a
autoComplete items selectedIndex =
    ul
        [ class "list-group cm-autocomplete" ]
    <|
        List.indexedMap (\index item -> li [ class (getClass (selectedIndex == index)) ] [ text item.text ]) items
