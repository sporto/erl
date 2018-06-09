module Main exposing (..)

import ErlTests
import Test exposing (..)


all : Test
all =
    describe "all"
        [ ErlTests.all
        ]
