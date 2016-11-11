module Main exposing (..)

import Html exposing (Html, div, input, text)
import Html.App as Html
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onSubmit)
import Components.TodoList exposing (todoList)
import Components.Todo exposing (Todo)


main =
    Html.beginnerProgram
        { model = model
        , view = view
        , update = update
        }


type alias Model =
    { todos : List Todo
    , userInput : String
    }


model : Model
model =
    Model
        [ { text = "Todo 1"
          , description = "decsctiption"
          , completed = False
          }
        , { text = "Todo 2"
          , description = "decsctiption2"
          , completed = True
          }
        ]
        ""


addTodo : String -> Todo
addTodo text =
    { text = text
    , description = "test"
    , completed = False
    }


type Msg
    = Change String
    | Submit
    | TodoClick


update : Msg -> Model -> Model
update msg model =
    case msg of
        Change updatedCommand ->
            { model | userInput = updatedCommand }

        Submit ->
            { model
                | todos = model.todos ++ [ addTodo model.userInput ]
                , userInput = ""
            }

        TodoClick ->
            { model | todos = List.map (\t -> { t | completed = True }) model.todos }


view : Model -> Html Msg
view model =
    div []
        [ Html.form [ class "cm-command", onSubmit Submit ]
            [ div [ class "form-group" ]
                [ div [ class "input-group" ]
                    [ div [ class "input-group-addon" ] [ text "" ]
                    , input [ class "form-control", value model.userInput, placeholder "Enter command", onInput Change ] []
                    ]
                ]
            ]
        , todoList model.todos TodoClick
        ]
