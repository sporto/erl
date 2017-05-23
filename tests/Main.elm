module Main exposing (..)

import Json.Encode exposing (Value)
import ErlTests
import Test exposing (..)


all : Test
all =
    describe "all"
        [ ErlTests.all
        ]
