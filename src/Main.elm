port module Main exposing (..)

import Html.App exposing (program)
import View exposing (view)
import State exposing (update, init, subscriptions)


main : Program Never
main =
    program
        { init = init
        , update = update
        , view = View.view
        , subscriptions = subscriptions
        }
