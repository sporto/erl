port module Main exposing (..)

import Test exposing (describe)
import Test.Runner.Node exposing (run)
import Json.Encode exposing (Value)
import ErlTests


main : Test.Runner.Node.TestProgram
main =
    run emit ErlTests.all


port emit : ( String, Value ) -> Cmd msg
