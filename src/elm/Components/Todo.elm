module Components.Todo exposing (..)

import Html exposing (Html, li, text, span)
import Html.Attributes exposing (class, style)
import Html.Events exposing (onClick)


type alias Todo =
    { id : String
    , text : String
    , description : String
    , completed : Bool
    }


getStyle : Bool -> List ( String, String )
getStyle completed =
    if completed then
        [ ( "opacity", "0.2" ) ]
    else
        [ ( "opacity", "1" ) ]


todo : (String -> a) -> Int -> Todo -> Html a
todo msg index todo =
    li
        [ class "list-group-item", style (getStyle todo.completed) ]
        [ span [ class "badge pull-left cm-todo-toggle", onClick <| msg todo.id ] [ text (toString index) ]
        , span [ class "badge" ] [ text "Project 1" ]
        , text todo.text
        ]
