module ErlTest where

import String
import Dict
import Graphics.Element exposing (Element)
import Erl

import ElmTest.Test exposing (test, Test, suite)
import ElmTest.Assertion exposing (assert, assertEqual)
import ElmTest.Runner.Element exposing (runDisplay)

-- PROTOCOL

testProtocolComplete =
  let
    input =
      "http://example.com:3000"
    actual =
      (Erl.parse input).protocol
    expected =
      "http"
  in
    test "Extracts the protocol"
      (assertEqual expected actual)

testProtocolExtract =
  let
    input =
      "http://example.com:3000"
    actual =
      Erl.extractProtocol input
    expected =
      "http"
  in
    test "Extracts the protocol"
      (assertEqual expected actual)

testProtocolExtractWhenMissing =
  let
    input =
      "example.com:3000"
    actual =
      Erl.extractProtocol input
    expected =
      ""
  in
    test "Returns empty when protocol is missing"
      (assertEqual expected actual)

-- PORT

testPortExtract: Test
testPortExtract =
  let
    inputs =
      [
        ("http://example.com:3000", 3000),
        ("http://example.com:3000/", 3000),
        ("http://example.com:3000/users", 3000),
        ("http://example.com", 0),
        ("http://example.com/users", 0)
      ]
    run (input, expected) =
      test "Extracts the port"
        (assertEqual expected (Erl.extractPort input))
  in
    suite "Extract port"
      (List.map run inputs)

testPortComplete: Test
testPortComplete =
  let
    inputs =
      [
        ("http://example.com:3000/users", 3000)
      ]
    run (input, expected) =
      test "Extracts the port"
        (assertEqual expected (Erl.parse input).port')
  in
    suite "Port"
      (List.map run inputs)


-- USERNAME

-- PASSWORD

-- PATH

-- HASH

testHashParse =
  let
    input =
      "/users/1/edit"
    actual =
      Erl.parseHash input
    expected =
      ["users", "1", "edit"]
  in
    test
      "Returns hash as list"
      (assertEqual expected actual)

testHashComplete =
  let
    input =
      "#/users/1"
    actual =
      (Erl.parse input).hash
    expected = 
      ["users", "1"]
  in
    test
      "Returns hash as list"
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

testQueryComplete: Test
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

-- suite : String -> List Test -> Test
individualTests: Test
individualTests = 
  suite "Individual Tests"
    [ 
      testHashComplete,
      testHashParse,
      testProtocolExtract,
      testProtocolExtractWhenMissing,
      testQueryComplete,
      testQueryKeyValues
    ]

testSuites: List Test
testSuites =
  [
    individualTests,
    testPortExtract,
    testPortComplete
  ]

testSuite: Test
testSuite =
  suite "All test"
    testSuites

main : Element
main = 
    runDisplay testSuite
