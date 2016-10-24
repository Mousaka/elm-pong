module Tests exposing (..)

import Test exposing (..)
import Expect
import Pong.State exposing (..)


all : Test
all =
    describe "tests"
        [ describe "howFarPastX tests"
            [ test "90 degree angle up gives 0 x correction" <|
                \() ->
                    Expect.equal 0 (howFarPastX (pi / 2) 11)
            , test "90 degree angle down gives 0 x correction" <|
                \() ->
                    Expect.equal 0 (howFarPastX -(pi / 2) 11)
            , test "45 degree angle and 1 y past gives 1 x correction" <|
                \() ->
                    Expect.equal 1 (howFarPastX (pi / 4) 1)
            , test "45 degree angle and 1 y past gives 1 x correction" <|
                \() ->
                    Expect.equal 11 (howFarPastX (pi / 4) 11)
            ]
        , describe "bounce angle"
            [ test "-45 degree in gives 45 degree out" <|
                \() ->
                    Expect.equal (pi / 4) (generalBounce -(pi / 4))
            , test "45 degree in gives -45 degree out" <|
                \() ->
                    Expect.equal (3 * pi / 4) (generalBounce -(3 * pi / 4))
            , test "-90 degree in gives 90 degree out" <|
                \() ->
                    Expect.equal (pi / 2) (generalBounce -(pi / 2))
            ]
        ]
