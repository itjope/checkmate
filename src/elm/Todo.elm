module Todo exposing (Id, Todo, todo, todoList)

import Html exposing (Html, li, text, span, ul)
import Html.Attributes exposing (class, style)
import Html.Events exposing (onClick)
import String


type alias Id =
    String


type alias Todo =
    { id : Id
    , text : String
    , description : String
    , completed : Bool
    , tags : List String
    }


getStyle : Bool -> List ( String, String )
getStyle completed =
    if completed then
        [ ( "opacity", "0.2" ) ]
    else
        [ ( "opacity", "1" ) ]


getBadges : List String -> List (Html a)
getBadges badges =
    List.map
        (\badge -> span [ class "badge" ] [ text badge ])
        badges


parseDescription : String -> String
parseDescription description =
    if String.length description < 1 then
        "(empty)"
    else
        description


todo : (Id -> a) -> (Todo -> a) -> Int -> Todo -> Html a
todo onToggleClick onTextClick index todo =
    li
        [ class "list-group-item", style (getStyle todo.completed) ]
    <|
        [ span [ class "badge pull-left cm-todo-toggle", onClick <| onToggleClick todo.id ] [ text (toString index) ] ]
            ++ getBadges todo.tags
            ++ [ span [ onClick <| onTextClick todo ] [ text <| parseDescription todo.description ] ]


todoList : List Todo -> (Id -> a) -> (Todo -> a) -> Html a
todoList todos onToggleClick onTextClick =
    ul
        [ class "list-group cm-todos" ]
    <|
        List.indexedMap (todo onToggleClick onTextClick) todos
