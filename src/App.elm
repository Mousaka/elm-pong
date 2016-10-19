module App exposing (..)

import Html.App exposing (program)
import Pong.View exposing (view)
import Pong.State exposing (update, init, subscriptions)


main : Program Never
main =
    program
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }
