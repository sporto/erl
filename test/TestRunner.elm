module Main exposing (..)

import ElmTest
import ErlTests


main =
    ElmTest.runSuite ErlTests.all
