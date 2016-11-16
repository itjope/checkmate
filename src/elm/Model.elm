module Model exposing (Model, getInitialModel)

import Types exposing (Flags, Todo)


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


type alias Model =
    { todos : List Todo
    , selected : Maybe Todo
    , userInput : String
    , cuid : String
    , cuidCounter : Int
    , autocompletes : List Todo
    , autocompleteSelectedIndex : Int
    }
