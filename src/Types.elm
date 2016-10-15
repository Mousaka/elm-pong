module Types exposing (..)

import Time exposing (Time)
import Keyboard exposing (..)


type alias Model =
    { player1 : Player
    , player2 : Player
    , ball : Ball
    }


type alias Player =
    { y : Float
    , x : Float
    , velocity : Float
    , direction : Direction
    , side : Side
    }


type alias Ball =
    { x : Float
    , y : Float
    , speed : Float
    , direction : Float
    }


type Direction
    = None
    | Up
    | Down


type Side
    = Left
    | Right


type Msg
    = TimeUpdate Time
    | KeyDown KeyCode
    | KeyUp KeyCode


type PlayerKey
    = One
    | Two
    | Greger
