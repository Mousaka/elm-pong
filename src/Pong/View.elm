module Pong.View exposing (..)

import Html exposing (..)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Pong.Types exposing (..)


-- VIEW


view : Model -> Html Msg
view model =
    let
        player1 =
            model.player1

        player2 =
            model.player2

        ball =
            model.ball
    in
        div []
            [ Svg.svg [ Svg.Attributes.viewBox "0 0 1400 1000", Svg.Attributes.width "700px", Svg.Attributes.height "500px" ]
                ([ background
                 , getRect player1.x player1.y
                 , getRect player2.x player2.y
                 , getBall ball.x ball.y
                 ]
                    ++ drawNet
                )
            , debugData player1
            , debugData player2
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


drawNet : List (Svg Msg)
drawNet =
    drawNetHelp 1000 []


drawNetHelp : Int -> List (Svg Msg) -> List (Svg Msg)
drawNetHelp yPos list =
    case yPos > 0 of
        True ->
            drawNetHelp (yPos - 50) (list ++ [ dotNet yPos ])

        False ->
            list


dotNet : Int -> Svg Msg
dotNet yPos =
    rect [ x "692", y (toString yPos), Svg.Attributes.width "16", Svg.Attributes.height "16", fill "white" ] []


getRect : Float -> Float -> Svg Msg
getRect xPos yPos =
    let
        xPos' =
            toString xPos

        yPos' =
            toString yPos
    in
        rect [ x xPos', y yPos', Svg.Attributes.width "15", Svg.Attributes.height "100", fill "white" ] []


getBall : Float -> Float -> Svg Msg
getBall xPos yPos =
    circle [ cx (toString xPos), cy (toString yPos), r "10", fill "white" ] []
