module Main exposing (..)

import Html.App as App
import Html exposing (Html, div, span, input, form)
import Dom
import String
import Task
import Todo exposing (Todo, Id, todoList)
import AutoComplete exposing (autoComplete)
import CommandInput exposing (commandInput)


main : Program (Flags)
main =
    App.programWithFlags
        { init = init
        , view = view
        , update = update
        , subscriptions = always Sub.none
        }


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( getInitialModel flags
    , Cmd.none
    )


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


type alias Flags =
    { cuid : String
    }


type KeyboardKey
    = Esc
    | Tab
    | Other


getKeyboardKey : Int -> KeyboardKey
getKeyboardKey keyCode =
    case keyCode of
        27 ->
            Esc

        9 ->
            Tab

        _ ->
            Other


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


getId : String -> Int -> String
getId cuid cuidCounter =
    cuid ++ toString cuidCounter


completeTodo : Id -> Todo -> Todo
completeTodo id todo =
    if todo.id == id then
        { todo | completed = not todo.completed }
    else
        todo


parseTodoDescription : String -> String
parseTodoDescription text =
    String.words text |> List.filter isNotTag |> String.join " "


parseTodoTags : String -> List String
parseTodoTags text =
    String.words text |> List.filter isTag |> List.map (String.dropLeft 1)


updateTodo : Id -> String -> Todo -> Todo
updateTodo id text todo =
    if todo.id == id then
        { todo
            | text = text
            , description = parseTodoDescription text
            , tags = parseTodoTags text
        }
    else
        todo


autocompleteTodo : String -> Todo -> Bool
autocompleteTodo userInput todo =
    String.startsWith userInput todo.text


getNextAutocompleteIndex : Int -> Int -> Int
getNextAutocompleteIndex currentIndex todosLength =
    if currentIndex < todosLength then
        currentIndex + 1
    else
        0


isNotTag : String -> Bool
isNotTag word =
    not (String.startsWith "#" word)


isTag : String -> Bool
isTag word =
    String.startsWith "#" word


addTodo : Id -> String -> Todo
addTodo id text =
    { id = id
    , text = text
    , description = parseTodoDescription text
    , completed = False
    , tags = parseTodoTags text
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Keystroke keyCode ->
            case getKeyboardKey keyCode of
                Esc ->
                    ( { model
                        | userInput = ""
                        , selected = Nothing
                        , autocompletes = []
                        , autocompleteSelectedIndex = -1
                      }
                    , Cmd.none
                    )

                Tab ->
                    ( { model
                        | autocompletes = List.filter (autocompleteTodo model.userInput) model.todos
                        , autocompleteSelectedIndex = getNextAutocompleteIndex model.autocompleteSelectedIndex (List.length model.autocompletes)
                      }
                    , Cmd.none
                    )

                Other ->
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
                        List.head <| List.drop model.autocompleteSelectedIndex model.autocompletes
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
                    Dom.focus "cm-command-input"
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


view : Model -> Html Msg
view model =
    div []
        [ commandInput model.userInput Submit Change Blur Keystroke
        , autoComplete model.autocompletes model.autocompleteSelectedIndex
        , todoList model.todos TodoToggleClick TodoTextClick
        ]
