module Tests where

import String
import Dict
import Erl

import ElmTest exposing (..)

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

-- USERNAME

-- PASSWORD

-- HOST

-- host must be a-z 0-9 and -

testHostExtract: Test
testHostExtract =
  let
    inputs =
      [
        ("http://foo.com", "foo.com"),
        ("http://12345.com", "12345.com"),
        ("http://api.foo.com", "api.foo.com"),
        ("http://api.foo.com/", "api.foo.com"),
        ("http://api.foo.com/users", "api.foo.com"),
        ("http://api.foo.com/users", "api.foo.com"),
        ("foo.com", "foo.com"),
        ("foo-.com", "foo-.com"),
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
        ("http://example.com:3000/users", 3000),
        ("http://example.com/users", 80)
      ]
    run (input, expected) =
      test "Extracts the port"
        (assertEqual expected (Erl.parse input).port')
  in
    suite "Port"
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
        ("/users/1/edit", ["users", "1", "edit"]),
        -- it decodes
        ("/us%2Fers/1/edit", ["us/ers", "1", "edit"])
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
        ("www.foo.com#/users/1?a=1", ["users", "1"]),
        -- it decodes
        ("#/us%2Fers/1", ["us/ers", "1"])
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
        ("users?a=1&b=2", Dict.empty |> Dict.insert "a" "1" |> Dict.insert "b" "2"),
        ("users?a%3F=1%26", Dict.empty |> Dict.insert "a?" "1&")
      ]
    run (input, expected) =
      test "Parses the query"
        (assertEqual expected (Erl.parse input).query)
  in
    suite "Query"
      (List.map run inputs)


-- TO STRING

testToString =
  let
    url1 =
      {
        protocol = "http",
        username = "",
        password = "",
        host = ["www", "foo", "com"],
        path = ["users", "1"],
        port' = 2000,
        fragment = ["a", "b"],
        query = Dict.empty |> Dict.insert "q" "1" |> Dict.insert "k" "2"
      }
    url2 =
      {
        protocol = "",
        username = "",
        password = "",
        host = [],
        port' = 0,
        path = [],
        fragment = ["a", "b"],
        query = Dict.empty |> Dict.insert "q" "1" |> Dict.insert "k" "2"
      }
    inputs = 
      [
        (url1, "http://www.foo.com:2000/users/1#a/b?k=2&q=1"),
        ({url1 | protocol = ""}, "www.foo.com:2000/users/1#a/b?k=2&q=1"),
        ({url1 | port' = 80}, "http://www.foo.com/users/1#a/b?k=2&q=1"),
        ({url1 | fragment = []}, "http://www.foo.com:2000/users/1?k=2&q=1"),
        ({url1 | query = Dict.empty}, "http://www.foo.com:2000/users/1#a/b"),
        -- encodes values in host
        ({url1 | host = ["aa/bb", "com"]}, "http://aa%2Fbb.com:2000/users/1#a/b?k=2&q=1"),
        -- encodes values in path
        ({url1 | path = ["aa/bb", "2"]}, "http://www.foo.com:2000/aa%2Fbb/2#a/b?k=2&q=1"),
        -- encodes values in fragment
        ({url1 | fragment = ["aa/bb", "2"]}, "http://www.foo.com:2000/users/1#aa%2Fbb/2?k=2&q=1"),
        -- encodes values in query
        ({url1 | query = Dict.empty |> Dict.insert "a/b" "c/d" }, "http://www.foo.com:2000/users/1#a/b?a%2Fb=c%2Fd"),
        (url2, "#a/b?k=2&q=1")
      ]
    run (input, expected) =
      test "Generates the url"
        (assertEqual expected (Erl.toString input))
  in
    suite "toString"
      (List.map run inputs)

testNew =
  let
    expected =
      {
        protocol = "",
        username = "",
        password = "",
        host = [],
        port' = 0,
        path = [],
        fragment = [],
        query = Dict.empty
      }
    actual =
      Erl.new
  in
    test "Generates an empty url"
      (assertEqual expected actual)

testQuerySet =
  let
    expected =
      Dict.empty |> Dict.insert "a" "1" |> Dict.insert "b" "2"
    actual =
      Erl.new
        |> Erl.setQuery "a" "1"
        |> Erl.setQuery "b" "2"
        |> .query
  in
    test "Sets the query"
      (assertEqual expected actual)

testQueryUnset =
  let
    expected =
      Dict.empty |> Dict.insert "a" "1"
    actual =
      Erl.new
        |> Erl.setQuery "a" "1"
        |> Erl.setQuery "b" "2"
        |> Erl.unsetQuery "b"
        |> .query
  in
    test "Unsets the query"
      (assertEqual expected actual)

testQueryClear =
  let
    expected =
      Dict.empty
    actual =
      Erl.new
        |> Erl.setQuery "a" "1"
        |> Erl.clearQuery
        |> .query
  in
    test "Cleans the query"
      (assertEqual expected actual)

-- suite : String -> List Test -> Test
all: Test
all = 
  suite "Tests"
    [ 
      testFragment,
      testFragmentExtract,
      testHost,
      testHostExtract,
      testNew,
      testPath,
      testPathExtract,
      testPort,
      testPortExtract,
      testProtocol,
      testProtocolExtract,
      testQuery,
      testQueryClear,
      testQueryExtract,
      testQuerySet,
      testQueryUnset,
      testToString
    ]
