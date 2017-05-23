module Main exposing (..)

import Json.Encode exposing (Value)
import ErlTests
import Erl.QueryTests
import Test exposing (..)


all : Test
all =
    describe "all"
        [ ErlTests.all
        , Erl.QueryTests.all
        ]
