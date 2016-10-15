module Key exposing (..)


type Key
    = Space
    | ArrowUp
    | ArrowDown
    | W
    | S
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

        87 ->
            W

        83 ->
            S

        _ ->
            Unknown
