module Main exposing (..)

import Html exposing (Html, div, input, text, span)
import Html.App as Html
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onSubmit)
import Components.TodoList exposing (todoList)
import Components.Todo exposing (Todo, Id)


main : Program (Flags)
main =
    Html.programWithFlags
        { init = init
        , view = view
        , update = update
        , subscriptions = always Sub.none
        }


type alias Flags =
    { cuid : String
    }


addTodo : Id -> String -> Todo
addTodo id text =
    { id = id
    , text = text
    , description = "test"
    , completed = False
    }


type Msg
    = Change String
    | Submit
    | TodoToggleClick Id
    | TodoTextClick Todo


type alias Model =
    { todos : List Todo
    , selected : Maybe Todo
    , userInput : String
    , cuid : String
    , cuidCounter : Int
    }


getInitialModel : Flags -> Model
getInitialModel flags =
    { todos = []
    , selected = Nothing
    , userInput = ""
    , cuid = flags.cuid
    , cuidCounter = 0
    }


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( getInitialModel flags
    , Cmd.none
    )


getId : String -> Int -> String
getId cuid cuidCounter =
    cuid ++ toString cuidCounter


completeTodo : String -> Todo -> Todo
completeTodo id todo =
    if todo.id == id then
        { todo | completed = not todo.completed }
    else
        todo


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Change updatedCommand ->
            ( { model | userInput = updatedCommand }, Cmd.none )

        Submit ->
            ( { model
                | todos = model.todos ++ [ addTodo (getId model.cuid model.cuidCounter) model.userInput ]
                , userInput = ""
                , cuidCounter = model.cuidCounter + 1
              }
            , Cmd.none
            )

        TodoToggleClick id ->
            ( { model
                | todos =
                    List.map
                        (completeTodo id)
                        model.todos
              }
            , Cmd.none
            )

        TodoTextClick todo ->
            ( { model
                | selected = Just todo
                , userInput = todo.text
              }
            , Cmd.none
            )


view : Model -> Html Msg
view model =
    div []
        [ Html.form [ class "cm-command", onSubmit Submit ]
            [ div [ class "form-group" ]
                [ div [ class "input-group" ]
                    [ div [ class "input-group-addon" ]
                        [ span [ class "glyphicon glyphicon-option-vertical" ] []
                        ]
                    , input [ class "form-control", value model.userInput, onInput Change ] []
                    ]
                ]
            ]
        , todoList model.todos TodoToggleClick TodoTextClick
        ]
