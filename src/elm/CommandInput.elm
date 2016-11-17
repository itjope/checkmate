module CommandInput exposing (commandInput)

import Json.Decode as Json
import Html exposing (Html, div, span, input, form)
import Html.Events exposing (onSubmit, onInput, onBlur, keyCode, on)
import Html.Attributes exposing (id, class, autocomplete, value)


onKeyUp : (Int -> msg) -> Html.Attribute msg
onKeyUp tagger =
    on "keyup" (Json.map tagger keyCode)


commandInput : String -> a -> (String -> a) -> a -> (Int -> a) -> Html a
commandInput userInput submitMsg inputMsg blurMsg keystrokeMsg =
    form [ class "cm-command", onSubmit submitMsg ]
        [ div [ class "form-group" ]
            [ div [ class "input-group" ]
                [ div [ class "input-group-addon" ]
                    [ span [ class "glyphicon glyphicon-option-vertical" ] []
                    ]
                , input
                    [ id "cm-command-input"
                    , class "form-control"
                    , autocomplete False
                    , value userInput
                    , onInput inputMsg
                    , onBlur blurMsg
                    , onKeyUp keystrokeMsg
                    ]
                    []
                ]
            ]
        ]
