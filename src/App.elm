module App exposing (..)

import Html exposing (program)
import Pong.View exposing (view)
import Pong.State exposing (update, init)
import Pong.Subscriptions exposing (subscriptions)


main =
    program
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }
