module Main exposing (..)

import Dom exposing (focus)
import Task exposing (Task)
import String exposing (startsWith)
import Html exposing (Html, div, input, text, span)
import Html.App as Html
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onSubmit, onBlur)
import Components.TodoList exposing (todoList)
import Components.AutoComplete exposing (autoComplete)
import Components.Todo exposing (Todo, Id)
import Json.Decode as Json


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
    | Blur
    | DomError Dom.Error
    | DomSuccess
    | Keystroke Int


type alias Model =
    { todos : List Todo
    , selected : Maybe Todo
    , userInput : String
    , cuid : String
    , cuidCounter : Int
    , autocompletes : List Todo
    , autocompleteSelectedIndex : Int
    }


getInitialModel : Flags -> Model
getInitialModel flags =
    { todos = []
    , selected = Nothing
    , userInput = ""
    , cuid = flags.cuid
    , cuidCounter = 0
    , autocompletes = []
    , autocompleteSelectedIndex = -1
    }


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( getInitialModel flags
    , Cmd.none
    )


getId : String -> Int -> String
getId cuid cuidCounter =
    cuid ++ toString cuidCounter


completeTodo : Id -> Todo -> Todo
completeTodo id todo =
    if todo.id == id then
        { todo | completed = not todo.completed }
    else
        todo


updateTodo : Id -> String -> Todo -> Todo
updateTodo id text todo =
    if todo.id == id then
        { todo | text = text }
    else
        todo


autocompleteTodo : String -> Todo -> Bool
autocompleteTodo userInput todo =
    startsWith userInput todo.text


getNextAutocompleteIndex : Int -> Int -> Int
getNextAutocompleteIndex currentIndex todosLength =
    if currentIndex < todosLength then
        currentIndex + 1
    else
        0


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Keystroke keyCode ->
            if keyCode == 27 then
                ( { model
                    | userInput = ""
                    , selected = Nothing
                    , autocompletes = []
                    , autocompleteSelectedIndex = -1
                  }
                , Cmd.none
                )
            else if keyCode == 9 then
                ( { model
                    | autocompletes = List.filter (autocompleteTodo model.userInput) model.todos
                    , autocompleteSelectedIndex = getNextAutocompleteIndex model.autocompleteSelectedIndex (List.length model.autocompletes)
                  }
                , Cmd.none
                )
            else
                ( { model
                    | autocompletes = []
                    , autocompleteSelectedIndex = -1
                  }
                , Cmd.none
                )

        Change value ->
            ( { model | userInput = value }, Cmd.none )

        Submit ->
            if model.autocompleteSelectedIndex > -1 then
                let
                    selectedTodo =
                        List.head (List.drop model.autocompleteSelectedIndex model.autocompletes)
                in
                    case selectedTodo of
                        Nothing ->
                            ( model
                            , Cmd.none
                            )

                        Just todo ->
                            ( { model
                                | selected = Just todo
                                , userInput = todo.text
                              }
                            , Cmd.none
                            )
            else
                case model.selected of
                    Nothing ->
                        ( { model
                            | todos = model.todos ++ [ addTodo (getId model.cuid model.cuidCounter) model.userInput ]
                            , userInput = ""
                            , cuidCounter = model.cuidCounter + 1
                          }
                        , Cmd.none
                        )

                    Just todo ->
                        ( { model
                            | userInput = ""
                            , selected = Nothing
                            , todos = List.map (updateTodo todo.id model.userInput) model.todos
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
            let
                cmd =
                    focus "cm-command-input"
                        |> Task.perform (\error -> DomError error) (\() -> DomSuccess)
            in
                ( { model
                    | selected = Just todo
                    , userInput = todo.text
                  }
                , cmd
                )

        Blur ->
            ( { model | selected = Nothing, userInput = "" }, Cmd.none )

        DomError error ->
            ( { model
                | userInput = "Failed to set focus"
              }
            , Cmd.none
            )

        DomSuccess ->
            ( model
            , Cmd.none
            )


onKeyUp : (Int -> msg) -> Html.Attribute msg
onKeyUp tagger =
    Html.Events.on "keyup" (Json.map tagger Html.Events.keyCode)


view : Model -> Html Msg
view model =
    div []
        [ Html.form [ class "cm-command", onSubmit Submit ]
            [ autoComplete model.autocompletes model.autocompleteSelectedIndex
            , div [ class "form-group" ]
                [ div [ class "input-group" ]
                    [ div [ class "input-group-addon" ]
                        [ span [ class "glyphicon glyphicon-option-vertical" ] []
                        ]
                    , input [ id "cm-command-input", class "form-control", autocomplete False, value model.userInput, onInput Change, onBlur Blur, onKeyUp Keystroke ] []
                    ]
                ]
            ]
        , todoList model.todos TodoToggleClick TodoTextClick
        ]
