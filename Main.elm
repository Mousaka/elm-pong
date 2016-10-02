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
    { player : Player
    , ball : Ball
    }


type alias Player =
    { y : Float
    , velocity : Float
    , direction : Direction
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


init : ( Model, Cmd Msg )
init =
    ( { player = initPlayer, ball = initBall }, Cmd.none )


initPlayer : Player
initPlayer =
    { y = 500, velocity = 0, direction = None }


initBall : Ball
initBall =
    { x = 350, y = 500, speed = 1, direction = -1 * pi / 4 }



-- UPDATE


type Msg
    = TimeUpdate Time
    | KeyDown KeyCode
    | KeyUp KeyCode


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        KeyDown code ->
            ( { model | player = keyDown code model.player }, Cmd.none )

        KeyUp _ ->
            ( { model | player = keyUp model.player }, Cmd.none )

        TimeUpdate t ->
            ( { model | player = applyPlayerPhysics t model.player, ball = applyBallPhysics t model.ball }, Cmd.none )


keyUp : Player -> Player
keyUp player =
    { player | direction = None }


keyDown : KeyCode -> Player -> Player
keyDown keyCode player =
    case fromCode keyCode of
        ArrowDown ->
            { player | direction = Down }

        ArrowUp ->
            { player | direction = Up }

        Space ->
            player

        Unknown ->
            player


ballMove dt speed direction =
    let
        x =
            dt * speed * cos direction

        y =
            dt * speed * sin direction
    in
        ( x, y )


applyBallPhysics : Time -> Ball -> Ball
applyBallPhysics dt ball =
    let
        ( x, y ) =
            ballMove dt ball.speed ball.direction

        newX =
            ball.x + x

        newY =
            ball.y + y
    in
        { ball | x = newX, y = newY }


applyPlayerPhysics : Time -> Player -> Player
applyPlayerPhysics dt player =
    case player.direction of
        Up ->
            { player
                | y =
                    if player.y <= 0 then
                        player.y
                    else
                        player.y - 7
            }

        Down ->
            { player
                | y =
                    if player.y >= 900 then
                        player.y
                    else
                        player.y + 7
            }

        None ->
            player



-- VIEW


view : Model -> Html Msg
view model =
    let
        player =
            model.player

        ball =
            model.ball
    in
        div []
            [ Svg.svg [ Svg.Attributes.viewBox "0 0 1400 1000", Svg.Attributes.width "700px", Svg.Attributes.height "500px" ]
                [ background
                , getRect player.y
                , getBall ball.x ball.y
                ]
            , debugData player
            ]


debugData : Player -> Html Msg
debugData player =
    div []
        [ Html.text ("velocity: " ++ (toString player.velocity))
        , Html.text ("y: " ++ (toString player.y))
        ]


background : Svg Msg
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


getRect : Float -> Svg Msg
getRect yPos =
    let
        yPos' =
            toString yPos
    in
        rect [ x "20", y yPos', Svg.Attributes.width "15", Svg.Attributes.height "100", fill "white" ] []


getBall : Float -> Float -> Svg Msg
getBall xPos yPos =
    circle [ cx (toString xPos), cy (toString yPos), r "10", fill "white" ] []
