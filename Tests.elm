module Tests where

import String
import Dict
import Erl

import ElmTest.Test exposing (test, Test, suite)
import ElmTest.Assertion exposing (assert, assertEqual)
import ElmTest.Runner.Element exposing (runDisplay)

-- PROTOCOL

testProtocol =
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

testPort: Test
testPort =
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

testHost: Test
testHost =
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

testPath: Test
testPath =
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

testFragmentExtract =
  let
    inputs =
      [
        ("#/users/1", "/users/1"),
        ("www.foo.com/hello#/users/1?a=1", "/users/1")
      ]
    run (input, expected) =
      test "Extracts the fragment"
        (assertEqual expected (Erl.extractFragment input))
  in
    suite "Fragment"
      (List.map run inputs)

testFragment =
  let
    inputs =
      [
        ("#/users/1", ["users", "1"]),
        ("www.foo.com#/users/1?a=1", ["users", "1"])
      ]
    run (input, expected) =
      test "Parses the fragment"
        (assertEqual expected (Erl.parse input).fragment)
  in
    suite "Fragment"
      (List.map run inputs)

-- QUERY

testQueryExtract =
  let
    inputs =
      [
        ("http://foo.com/users?a=1", "a=1")
      ]
    run (input, expected) =
      test "Extracts the query"
        (assertEqual expected (Erl.extractQuery input))
  in
    suite "Query"
      (List.map run inputs)

testQuery =
  let
    inputs = 
      [
        ("users?a=1&b=2", Dict.empty |> Dict.insert "a" "1" |> Dict.insert "b" "2")
      ]
    run (input, expected) =
      test "Parses the query"
        (assertEqual expected (Erl.parse input).query)
  in
    suite "Query"
      (List.map run inputs)

-- suite : String -> List Test -> Test
all: Test
all = 
  suite "Tests"
    [ 
      testFragment,
      testFragmentExtract,
      testHost,
      testHostExtract,
      testPath,
      testPathExtract,
      testPort,
      testPortExtract,
      testProtocol,
      testProtocolExtract,
      testQuery,
      testQueryExtract
    ]
