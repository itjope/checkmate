module Types exposing (..)


type alias Flags =
    { cuid : String
    }


type alias Id =
    String


type alias Todo =
    { id : Id
    , text : String
    , description : String
    , completed : Bool
    , tags : List String
    }
