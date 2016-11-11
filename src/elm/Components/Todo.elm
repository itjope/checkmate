module Components.Todo exposing (..)

import Html exposing (..)
import Html.Attributes exposing (class, style)
import Html.Events exposing (onClick)


type alias Todo =
    { text : String
    , description : String
    , completed : Bool
    }


getStyle : Bool -> List ( String, String )
getStyle completed =
    if completed then
        [ ( "opacity", "0.3" ) ]
    else
        [ ( "opacity", "1" ) ]


todo : a -> Todo -> Html a
todo msg todo =
    li
        [ class "list-group-item", style (getStyle todo.completed), onClick msg ]
        [ text todo.text ]
