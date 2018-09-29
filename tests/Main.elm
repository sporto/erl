module Main exposing (all)

import Erl.ParsersTests
import Erl.QueryTests
import ErlTests
import Test exposing (..)


all : Test
all =
    describe "all"
        [ ErlTests.all
        , Erl.QueryTests.all
        , Erl.ParsersTests.all
        ]
