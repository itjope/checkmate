module Components.Todo exposing (todo)

import Html exposing (Html, li, text, span)
import Html.Attributes exposing (class, style)
import Html.Events exposing (onClick)
import Types exposing (Id, Todo)


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


todo : (Id -> a) -> (Todo -> a) -> Int -> Todo -> Html a
todo todoToggleClick todoTextClick index todo =
    li
        [ class "list-group-item", style (getStyle todo.completed) ]
    <|
        [ span [ class "badge pull-left cm-todo-toggle", onClick <| todoToggleClick todo.id ] [ text (toString index) ] ]
            ++ getBadges todo.tags
            ++ [ span [ onClick <| todoTextClick todo ] [ text todo.description ] ]
