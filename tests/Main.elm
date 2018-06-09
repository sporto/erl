module Main exposing (..)

import ErlTests
import Erl.QueryTests
import Test exposing (..)


all : Test
all =
    describe "all"
        [ ErlTests.all
        , Erl.QueryTests.all
        ]
