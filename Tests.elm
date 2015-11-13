module Tests where

import String
import Dict
import Erl

import ElmTest.Test exposing (test, Test, suite)
import ElmTest.Assertion exposing (assert, assertEqual)
import ElmTest.Runner.Element exposing (runDisplay)

-- UTILS

testRightFrom =
  let
    inputs =
      [
        ("", "#", ""),
        ("aa#bb", "#", "bb"),
        ("aa#bb#cc", "#", "cc"),
        ("aa", "#", ""),
        ("#bb", "#", "bb"),
        ("aa#", "#", "")
      ]
    run (input, delimiter, expected) =
      test "Parses the path"
        (assertEqual expected (Erl.rightFrom delimiter input))
  in
    suite "rightFrom"
      (List.map run inputs)

-- PROTOCOL

testProtocolComplete =
  let
    inputs =
      [
        ("http://example.com:3000", "http")
      ]
    run (input, expected) =
      test "Protocol"
        (assertEqual expected (Erl.parse input).protocol)
  in
    suite "Protocol"
      (List.map run inputs)

testProtocolExtract =
  let
    inputs =
      [
        ("http://example.com:3000", "http"),
        ("https://example.com:3000", "https"),
        ("example.com:3000", "")
      ]
    run (input, expected) =
      test "Protocol"
        (assertEqual expected (Erl.extractProtocol input))
  in
    suite "Extract protocol"
      (List.map run inputs)

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
        ("http://example.com", 80),
        ("http://example.com/users", 80)
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

-- HOST

testHostExtract: Test
testHostExtract =
  let
    inputs =
      [
        ("http://foo.com", "foo.com"),
        ("http://api.foo.com", "api.foo.com"),
        ("http://api.foo.com/", "api.foo.com"),
        ("http://api.foo.com/users", "api.foo.com"),
        ("foo.com", "foo.com"),
        ("foo.com/users", "foo.com"),
        ("api.foo.com", "api.foo.com"),
        ("users/1/edit", ""),
        ("users/index.html", "")
      ]
    run (input, expected) =
      test ("Extracts host " ++ input)
        (assertEqual expected (Erl.extractHost input))
  in
    suite "Extract host"
      (List.map run inputs)

testHostComplete: Test
testHostComplete =
  let
    inputs =
      [
        ("http://www.foo.com/users" , ["www", "foo", "com"])
      ]
    run (input, expected) =
      test ("Parses host in " ++ input)
        (assertEqual expected (Erl.parse input).host)
  in
    suite "Parses host"
      (List.map run inputs)

-- PATH

testPathExtract: Test
testPathExtract =
  let
    inputs =
      [
        ("http://foo.com/users/index.html", "/users/index.html"),
        ("foo.com/users/index.html", "/users/index.html"),
        ("/users/index.html", "/users/index.html"),
        ("users/index.html", "users/index.html"),
        ("users/index.html#xyz", "users/index.html"),
        ("users/index.html?a=1", "users/index.html")
      ]
    run (input, expected) =
      test ("Extracts path " ++ input)
        (assertEqual expected (Erl.extractPath input))
  in
    suite "Extract path"
      (List.map run inputs)

testPathParse: Test
testPathParse =
  let
    inputs =
      [
        ("/users/index.html", ["users", "index.html"]),
        ("/users/1/edit", ["users", "1", "edit"])
      ]
    run (input, expected) =
      test "Parses the path"
        (assertEqual expected (Erl.parsePath input))
  in
    suite "Path"
      (List.map run inputs)

testPathComplete: Test
testPathComplete =
  let
    inputs =
      [
        ("http://foo.com/users/index.html?a=1", ["users", "index.html"]),
        ("/users/1/edit", ["users", "1", "edit"])
      ]
    run (input, expected) =
      test "Parses the path"
        (assertEqual expected (Erl.parse input).path)
  in
    suite "Path"
      (List.map run inputs)

-- FRAGMENT

testFragmentParse =
  let
    input =
      "/users/1/edit"
    actual =
      Erl.parseFragment input
    expected =
      ["users", "1", "edit"]
  in
    test
      "Returns hash as list"
      (assertEqual expected actual)

testFragmentComplete =
  let
    input =
      "#/users/1"
    actual =
      (Erl.parse input).fragment
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
all: Test
all = 
  suite "Tests"
    [ 
      testFragmentComplete,
      testFragmentParse,
      testHostComplete,
      testHostExtract,
      testPathComplete,
      testPathExtract,
      testPathParse,
      testPortComplete,
      testPortExtract,
      testProtocolComplete,
      testProtocolExtract,
      testProtocolExtractWhenMissing,
      testQueryComplete,
      testQueryKeyValues,
      testRightFrom
    ]
