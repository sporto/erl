module ErlTest where

import String
import Dict
import Graphics.Element exposing (Element)
import Erl

import ElmTest.Test exposing (test, Test, suite)
import ElmTest.Assertion exposing (assert, assertEqual)
import ElmTest.Runner.Element exposing (runDisplay)

testHash =
  let
    input    = "#/users/1"
    actual   = (Erl.parse input).hash
    expected = ["users", "1"]
  in
    test
      "Returns path as list"
      (assertEqual expected actual)

-- QUERY
testQueryKeyValues =
  let
    input =
      "a=1&b=2"
    actual =
      Erl.parseQuery input
    expected =
      Dict.empty
        |> Dict.insert "a" "1"
        |> Dict.insert "b" "2"
  in
    test
      "Returns the correct dict from a query string"
      (assertEqual expected actual)

testQueryComplete =
  let
    input =
      "users?a=1&b=2"
    actual =
      (Erl.parse input).query
    expected = 
      Dict.empty
        |> Dict.insert "a" "1"
        |> Dict.insert "b" "2"
  in
    test
      "Returns query string pairs"
      (assertEqual expected actual)


tests : Test
tests = 
  suite "A Test Suite" 
    [ 
      testHash,
      testQueryKeyValues,
      testQueryComplete
    ]

main : Element
main = 
    runDisplay tests
