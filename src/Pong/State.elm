port module Pong.State exposing (init, update, subscriptions)

import Pong.Types exposing (..)
import Key exposing (..)
import Keyboard exposing (..)
import Time exposing (..)
import AnimationFrame


port alarm : () -> Cmd msg


init : ( Model, Cmd Msg )
init =
    ( { player1 = initPlayer 30 Left, player2 = initPlayer 1350 Right, ball = initBall }, alarm () )


initPlayer : Float -> Side -> Player
initPlayer startX startSide =
    { x = startX, y = 500, velocity = 0, direction = None, side = startSide }


initBall : Ball
initBall =
    { x = 350, y = 400, speed = 1, direction = pi / 4 }



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( updateHelper msg model, Cmd.none )


updateHelper : Msg -> Model -> Model
updateHelper msg model =
    case msg of
        KeyDown code ->
            case oneOrTwo (fromCode code) of
                One ->
                    { model | player1 = keyDown code model.player1 }

                Two ->
                    { model | player2 = keyDown code model.player2 }

                Greger ->
                    model

        KeyUp code ->
            case oneOrTwo (fromCode code) of
                One ->
                    { model | player1 = keyUp model.player1 }

                Two ->
                    { model | player2 = keyUp model.player2 }

                Greger ->
                    model

        TimeUpdate dt ->
            { model | player1 = applyPlayerPhysics dt model.player1, player2 = applyPlayerPhysics dt model.player2, ball = updateBall dt model.ball model.player1 model.player2 }


oneOrTwo : Key -> PlayerKey
oneOrTwo code =
    case code of
        ArrowUp ->
            Two

        ArrowDown ->
            Two

        W ->
            One

        S ->
            One

        Space ->
            Greger

        Unknown ->
            Greger


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

        W ->
            { player | direction = Up }

        S ->
            { player | direction = Down }

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


updateBall : Time -> Ball -> Player -> Player -> Ball
updateBall dt ball player1 player2 =
    ball |> bouncePlayer player1 |> bouncePlayer player2 |> bounceEnvironment |> applyBallPhysics dt


bouncePlayer : Player -> Ball -> Ball
bouncePlayer player ball =
    let
        px =
            player.y

        py =
            player.y
    in
        if hitPlayer player ball then
            bounce ball
        else
            ball


bounceEnvironment : Ball -> Ball
bounceEnvironment ball =
    let
        roof =
            0

        floor =
            1000
    in
        if hitRoof ball || hitFloor ball then
            bounce ball
        else
            ball


bounce : Ball -> Ball
bounce ball =
    { ball | direction = ball.direction + pi / 2 }


hitWall : Ball -> Bool
hitWall ball =
    ball.x > 1400


hitRoof : Ball -> Bool
hitRoof ball =
    ball.y <= 0


hitFloor : Ball -> Bool
hitFloor ball =
    ball.y >= 1000


hitPlayer : Player -> Ball -> Bool
hitPlayer player ball =
    case player.side of
        Left ->
            (ball.y > player.y) && ball.y < (player.y + 100) && (ball.x < player.x)

        Right ->
            (ball.y > player.y) && ball.y < (player.y + 100) && (ball.x > player.x)


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



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ AnimationFrame.diffs TimeUpdate
        , Keyboard.downs KeyDown
        , Keyboard.ups KeyUp
        ]
