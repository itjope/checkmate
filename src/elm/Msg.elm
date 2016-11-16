module Msg exposing (Msg(..))

import Types exposing (Id, Todo)
import Dom exposing (Error)


type Msg
    = Change String
    | Submit
    | TodoToggleClick Id
    | TodoTextClick Todo
    | Blur
    | DomError Error
    | DomSuccess
    | Keystroke Int
