module Main exposing (..)

import Html.App as App
import Html exposing (Html, div, span, input, form)
import Dom
import String
import Task
import Todo exposing (Todo, Id, todoList)
import AutoComplete exposing (AutocompleteItem, autoComplete)
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
    , autocompletes : List AutocompleteItem
    , autocompleteSelectedIndex : Int
    , commands : List String
    }


type alias Flags =
    { cuid : String
    }


type KeyboardKey
    = Esc
    | Tab
    | Other


type SubmitType
    = AutocompleteTodo
    | AutocompleteCommand
    | Command CommandName
    | Input


type CommandName
    = Clear
    | Complete (List Int)


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
    , commands = [ "/clear", "/complete" ]
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


autocompleteTodo : String -> Todo -> Maybe AutocompleteItem
autocompleteTodo userInput todo =
    if String.startsWith (String.toLower userInput) (String.toLower todo.text) then
        Just
            { key = todo.id
            , text = todo.text
            }
    else
        Nothing


autocompleteCommand : String -> String -> Maybe AutocompleteItem
autocompleteCommand userInput command =
    if String.startsWith userInput command then
        Just
            { key = command
            , text = command
            }
    else
        Nothing


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


submitType : Model -> SubmitType
submitType model =
    if model.autocompleteSelectedIndex > -1 && String.startsWith "/" model.userInput then
        AutocompleteCommand
    else if model.autocompleteSelectedIndex > -1 then
        AutocompleteTodo
    else if String.startsWith "/clear" model.userInput then
        Command Clear
    else if String.startsWith "/complete" model.userInput then
        let
            indexes =
                String.words model.userInput
                    |> List.map
                        (\word ->
                            let
                                res =
                                    String.toInt word
                            in
                                case res of
                                    Ok number ->
                                        number

                                    Err error ->
                                        -1
                        )
                    |> List.filter (\number -> number > -1)
        in
            Command (Complete indexes)
    else
        Input


selectedId : Maybe Todo -> String
selectedId selectedTodo =
    case selectedTodo of
        Just todo ->
            todo.id

        Nothing ->
            ""


todoFromSelectedAutocomplete : List AutocompleteItem -> Int -> List Todo -> Maybe Todo
todoFromSelectedAutocomplete autocompletes autocompleteIndex todos =
    let
        selectedItem =
            List.head <| List.drop autocompleteIndex autocompletes
    in
        case selectedItem of
            Just selected ->
                List.head <| List.filter (\todo -> todo.id == selected.key) todos

            Nothing ->
                Nothing


commandFromSelectedAutocomplete : List AutocompleteItem -> Int -> List String -> Maybe String
commandFromSelectedAutocomplete autocompletes autocompleteIndex commands =
    let
        selectedItem =
            List.head <| List.drop autocompleteIndex autocompletes
    in
        case selectedItem of
            Just selected ->
                List.head <| List.filter (\command -> command == selected.key) commands

            Nothing ->
                Nothing


autocomplete : Model -> List AutocompleteItem
autocomplete model =
    if String.startsWith "/" model.userInput then
        List.filterMap (autocompleteCommand model.userInput) model.commands
    else
        List.filterMap (autocompleteTodo model.userInput) model.todos


focusCommandInput : Cmd Msg
focusCommandInput =
    Dom.focus "cm-command-input"
        |> Task.perform (\error -> DomError error) (\() -> DomSuccess)


removeAtIndexes : List Int -> List Todo -> List Todo
removeAtIndexes indexes todos =
    let
        idsToRemove =
            List.indexedMap
                (\index todo ->
                    { id = todo.id
                    , remove = List.member index indexes
                    }
                )
                todos
                |> List.filter (\todo -> todo.remove)
                |> List.map (\todo -> todo.id)
    in
        List.filter (\todo -> List.member todo.id idsToRemove == False) todos


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
                        | autocompletes = autocomplete model
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
            case submitType model of
                AutocompleteTodo ->
                    let
                        selectedTodo =
                            todoFromSelectedAutocomplete model.autocompletes model.autocompleteSelectedIndex model.todos
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

                AutocompleteCommand ->
                    let
                        selectedCommand =
                            commandFromSelectedAutocomplete model.autocompletes model.autocompleteSelectedIndex model.commands
                    in
                        case selectedCommand of
                            Nothing ->
                                ( model
                                , Cmd.none
                                )

                            Just command ->
                                ( { model
                                    | userInput = command ++ " "
                                  }
                                , Cmd.none
                                )

                Command name ->
                    case name of
                        Clear ->
                            ( { model
                                | todos = List.filter (\todo -> todo.completed == False) model.todos
                                , userInput = ""
                              }
                            , Cmd.none
                            )

                        Complete indexes ->
                            ( { model
                                | todos = removeAtIndexes indexes model.todos
                                , userInput = ""
                              }
                            , Cmd.none
                            )

                Input ->
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
            , focusCommandInput
            )

        TodoTextClick todo ->
            ( { model
                | selected = Just todo
                , userInput = todo.text
              }
            , focusCommandInput
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
        , todoList model.todos (selectedId model.selected) TodoToggleClick TodoTextClick
        ]
