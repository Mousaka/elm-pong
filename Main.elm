module Main exposing (..)

import Html.App exposing (program)
import Html exposing (..)
import Keyboard exposing (..)
import Key exposing (..)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import AnimationFrame
import Time exposing (Time)


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
    { y : Float
    , velocity : Float
    }


init : ( Model, Cmd Msg )
init =
    ( { y = 500, velocity = 0 }, Cmd.none )



-- UPDATE


type Msg
    = TimeUpdate Time
    | KeyDown KeyCode
    | KeyUp KeyCode


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        KeyDown code ->
            ( keyDown code model, Cmd.none )

        KeyUp _ ->
            ( { model | velocity = 0 }, Cmd.none )

        TimeUpdate t ->
            ( applyPhysics t model, Cmd.none )


keyDown : KeyCode -> Model -> Model
keyDown keyCode model =
    let
        maxSpeed =
            4
    in
        case fromCode keyCode of
            ArrowUp ->
                if model.velocity > -maxSpeed then
                    { model | velocity = model.velocity - 1 }
                else
                    model

            ArrowDown ->
                if model.velocity < maxSpeed then
                    { model | velocity = model.velocity + 1 }
                else
                    model

            Space ->
                model

            Unknown ->
                model


applyPhysics : Time -> Model -> Model
applyPhysics dt model =
    { model | y = model.y + (model.velocity * dt) }



-- VIEW


view model =
    div []
        [ Svg.svg [ Svg.Attributes.viewBox "0 0 1400 1000", Svg.Attributes.width "700px", Svg.Attributes.height "500px" ]
            [ background
            , getRect model.y
            ]
        , debugData model
        ]


debugData model =
    div []
        [ Html.text ("velocity: " ++ (toString model.velocity))
        , Html.text ("y: " ++ (toString model.y))
        ]


background =
    rect [ x "0", y "0", Svg.Attributes.width "1400", Svg.Attributes.height "1000", fill "black" ] []



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ AnimationFrame.diffs TimeUpdate
        , Keyboard.downs KeyDown
        , Keyboard.ups KeyUp
        ]



-- FUNCTIONS


getRect yPos =
    let
        yPos' =
            toString yPos
    in
        rect [ x "20", y yPos', Svg.Attributes.width "15", Svg.Attributes.height "100", fill "white" ] []
