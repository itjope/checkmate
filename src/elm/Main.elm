module Main exposing (..)

import Html.App as Html
import Update exposing (update)
import Model exposing (Model)
import Msg exposing (Msg(..))
import Types exposing (Flags, Id, Todo)
import View exposing (view)


main : Program (Flags)
main =
    Html.programWithFlags
        { init = init
        , view = view
        , update = update
        , subscriptions = always Sub.none
        }


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( Model.getInitialModel flags
    , Cmd.none
    )
