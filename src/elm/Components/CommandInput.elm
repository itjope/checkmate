module Components.CommandInput exposing (commandInput)

import Html exposing (Html, form, div, span, input)
import Html.Attributes exposing (class, autocomplete, value, id)
import Html.Events exposing (onInput, onSubmit, onBlur)
import Json.Decode as Json
import Msg exposing (Msg(..))


onKeyUp : (Int -> msg) -> Html.Attribute msg
onKeyUp tagger =
    Html.Events.on "keyup" (Json.map tagger Html.Events.keyCode)


commandInput : String -> Html Msg
commandInput userInput =
    Html.form [ class "cm-command", onSubmit Submit ]
        [ div [ class "form-group" ]
            [ div [ class "input-group" ]
                [ div [ class "input-group-addon" ]
                    [ span [ class "glyphicon glyphicon-option-vertical" ] []
                    ]
                , input [ id "cm-command-input", class "form-control", autocomplete False, value userInput, onInput Change, onBlur Blur, onKeyUp Keystroke ] []
                ]
            ]
        ]
