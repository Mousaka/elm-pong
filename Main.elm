module Main exposing (..)

import Html.App exposing (program)
import Html exposing (..)
import Keyboard exposing (..)
import Key exposing (..)
import Svg exposing (..)
import Svg.Attributes exposing (..)

main : Program Never
main =
    program
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }

-- MODEl

type alias Model =
  {y: Int}

init : (Model, Cmd Msg)
init =
  ({y = 500}, Cmd.none)


-- UPDATE
type Msg = KeyDown KeyCode

update: Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    KeyDown code ->
      (keyDown code model, Cmd.none)

keyDown : KeyCode -> Model -> Model
keyDown keyCode model =
  case fromCode keyCode of
    ArrowUp ->
      if model.y > 0 then {model | y = model.y - 10} else model
    ArrowDown ->
      if model.y < 900 then {model | y = model.y + 10} else model
    Space -> model
    Unknown -> model

-- VIEW

view model =
  div [] [Svg.svg [Svg.Attributes.viewBox "0 0 1400 1000", Svg.Attributes.width "700px", Svg.Attributes.height "500px"]
      [ background
      , getRect model.y]
      ,Html.text (toString model.y)]


background =
  rect [x "0", y "0", Svg.Attributes.width "1400", Svg.Attributes.height "1000", fill "black"] []
-- SUBSCRIPTIONS

subscriptions: Model -> Sub Msg
subscriptions model =
  Sub.batch
    [Keyboard.downs KeyDown]

-- FUNCTIONS

getRect yPos =
  let
    yPos' = toString yPos
  in
  rect [ x "20", y yPos', Svg.Attributes.width "15", Svg.Attributes.height "100", fill "white" ] []
