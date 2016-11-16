module Update exposing (update)

import Dom
import Model exposing (Model)
import Msg exposing (Msg(..))
import Types exposing (Id, Todo)
import String
import Task


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
