
module Key exposing (..)


type Key
    = Space
    | ArrowUp
    | ArrowDown
    | Unknown


fromCode : Int -> Key
fromCode keyCode =
    case keyCode of
        32 ->
            Space

        38 ->
            ArrowUp

        40 ->
            ArrowDown

        _ ->
          Unknown
