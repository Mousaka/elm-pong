module Pong.Subscriptions exposing (subscriptions)

import AnimationFrame
import Keyboard exposing (..)
import Pong.Types exposing (..)


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ AnimationFrame.diffs TimeUpdate
        , Keyboard.downs KeyDown
        , Keyboard.ups KeyUp
        ]
