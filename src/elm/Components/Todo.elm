module Components.Todo exposing (todo)

import Html exposing (Html, li, text, span)
import Html.Attributes exposing (class, style)
import Html.Events exposing (onClick)
import Types exposing (Id, Todo)
import Msg exposing (Msg(..))


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


todo : Int -> Todo -> Html Msg
todo index todo =
    li
        [ class "list-group-item", style (getStyle todo.completed) ]
    <|
        [ span [ class "badge pull-left cm-todo-toggle", onClick <| TodoToggleClick todo.id ] [ text (toString index) ] ]
            ++ getBadges todo.tags
            ++ [ span [ onClick <| TodoTextClick todo ] [ text todo.description ] ]
