module Components.Todo exposing (..)

import Html exposing (..)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)


type alias Todo =
    { text : String
    , description : String
    , completed : Bool
    }


todo : a -> Todo -> Html a
todo click model =
    li
        [ class "list-group-item", onClick click ]
        [ text model.text ]
