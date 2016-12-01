port module Main exposing (..)

import Json.Encode as Json
import Json.Decode as JsonDecode
import Html exposing (Html, div, span, input, form, programWithFlags)
import Dom
import String
import Task
import Todo exposing (Todo, Id, todoList)
import AutoComplete exposing (AutocompleteItem, autoComplete)
import CommandInput exposing (commandInput)


port saveTodosToPouch : String -> Cmd msg


port pouchSaveSuccess : (String -> msg) -> Sub msg


port pouchSaveError : (String -> msg) -> Sub msg


port getTodosFromPouch : String -> Cmd msg


port pouchGetSuccess : (String -> msg) -> Sub msg


port pouchGetError : (String -> msg) -> Sub msg


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ pouchSaveSuccess PouchSaveSuccess
        , pouchSaveError PouchSaveError
        , pouchGetSuccess PouchGetSuccess
        , pouchGetError PouchGetError
        ]


main =
    programWithFlags
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( getInitialModel flags
    , getTodosFromPouch ""
    )


type Msg
    = Change String
    | Submit
    | TodoToggleClick Id
    | TodoTextClick Todo
    | Blur
    | NoOp
    | Keystroke Int
    | PouchSaveSuccess String
    | PouchSaveError String
    | PouchGetSuccess String
    | PouchGetError String


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
isTag =
    String.startsWith "#"


isCommand : String -> Bool
isCommand =
    String.startsWith "/"


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
    if model.autocompleteSelectedIndex > -1 && isCommand model.userInput then
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
    if isCommand model.userInput then
        List.filterMap (autocompleteCommand model.userInput) model.commands
    else
        List.filterMap (autocompleteTodo model.userInput) model.todos


always msg =
    \x -> msg


focusCommandInput : Cmd Msg
focusCommandInput =
    Task.attempt (always NoOp) (Dom.focus "cm-command-input")


completeAtIndexes : List Int -> List Todo -> List Todo
completeAtIndexes indexes todos =
    List.indexedMap
        (\index todo ->
            ({ todo
                | completed = List.member index indexes
             }
            )
        )
        todos


todosEncoder todos =
    Json.list (List.map todoEncoder todos)


todoEncoder todo =
    Json.object
        [ ( "_id", Json.string todo.id )
        , ( "text", Json.string todo.text )
        , ( "completed", Json.bool todo.completed )
        ]


createTodo id text completed =
    let
        todo =
            addTodo id text
    in
        ({ todo | completed = completed })


todoDecoder =
    JsonDecode.map3 createTodo (JsonDecode.field "_id" JsonDecode.string) (JsonDecode.field "text" JsonDecode.string) (JsonDecode.field "completed" JsonDecode.bool)


todosDecoder =
    JsonDecode.list todoDecoder


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        PouchSaveSuccess result ->
            ( model
            , Cmd.none
            )

        PouchSaveError result ->
            ( model
            , Cmd.none
            )

        PouchGetSuccess result ->
            case JsonDecode.decodeString todosDecoder <| result of
                Err msg ->
                    ( model
                    , Cmd.none
                    )

                Ok todos ->
                    ( { model | todos = todos }
                    , Cmd.none
                    )

        PouchGetError result ->
            ( model
            , Cmd.none
            )

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
                    let
                        autocompletes =
                            autocomplete model

                        firstAutocomplete =
                            List.head autocompletes
                    in
                        if List.length autocompletes == 1 && isCommand model.userInput then
                            case firstAutocomplete of
                                Just command ->
                                    ( { model
                                        | autocompletes = []
                                        , autocompleteSelectedIndex = -1
                                        , userInput = command.text ++ " "
                                      }
                                    , Cmd.none
                                    )

                                Nothing ->
                                    ( model
                                    , Cmd.none
                                    )
                        else
                            ( { model
                                | autocompletes = autocompletes
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
                                | todos = completeAtIndexes indexes model.todos
                                , userInput = ""
                              }
                            , Cmd.none
                            )

                Input ->
                    case model.selected of
                        Nothing ->
                            let
                                todo =
                                    addTodo (getId model.cuid model.cuidCounter) model.userInput
                            in
                                ( { model
                                    | todos = model.todos ++ [ todo ]
                                    , userInput = ""
                                    , cuidCounter = model.cuidCounter + 1
                                  }
                                , saveTodosToPouch (Json.encode 1 (todosEncoder [ todo ]))
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

        NoOp ->
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
