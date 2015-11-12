module Erl where

import Dict
import String exposing (..)
import Regex
import Debug

type alias Url = {
  domain: List String,
  hash: List String,
  password: String,
  path: List String,
  port': String,
  protocol: String,
  query: Dict.Dict String String,
  username: String
}

-- PROTOCOL

extractProtocol: String -> String
extractProtocol str =
  let
    parts =
      split "://" str
  in
    case List.length parts of
      1 ->
        ""
      _ ->
        Maybe.withDefault "" (List.head parts)

-- PORT

extractPort: String -> String
extractPort str =
  let
    rx =
      Regex.regex "(?:)\\d+"
    res =
      Regex.find (Regex.AtMost 1) rx str
  in
    res
      |> List.map .match
      |> List.head
      |> Maybe.withDefault ""

{--
      |> toInt
      |> Result.toMaybe
      |> Maybe.withDefault 0
--}
-- HASH

extractHash: String -> String
extractHash str =
  let
    parts = split "#" str
    maybeFirst = List.head (List.drop 1 parts)
  in
    Maybe.withDefault "" maybeFirst

parseHash: String -> List String
parseHash str =
  let
    list = split "/" str
    notEmpty = \x -> not (isEmpty x)
  in
    List.filter notEmpty list

hashFromAll: String -> List String
hashFromAll str =
  parseHash (extractHash str)

-- QUERY

-- "../foo?query=1" --> "query=1"
extractQueryString: String -> String
extractQueryString str =
  let
    parts = split "?" str
    maybeFirst = List.head (List.drop 1 parts)
  in
    Maybe.withDefault "" maybeFirst

-- "a=1" --> ("a", "1")
queryStringElementToTuple: String -> (String, String)
queryStringElementToTuple element =
  let
    splitted =
      split "=" element
    first l =
      Maybe.withDefault "" (List.head l)
    second l =
      Maybe.withDefault "" (List.head (List.drop 1 l))
  in
    (first splitted, second splitted)

-- "a=1&b=2" --> [("a", "1"), ("b", "2")]
queryTuples: String -> List (String, String)
queryTuples queryString =
  let
    splitted =
      split "&" queryString
  in
    List.map queryStringElementToTuple splitted

parseQuery: String -> Dict.Dict String String
parseQuery str =
  Dict.fromList (queryTuples str)

queryFromAll: String -> Dict.Dict String String
queryFromAll all =
  all
    |> extractQueryString
    |> parseQuery

-- MAIN

parse: String -> Url
parse str =
  {
    domain = [],
    hash = (hashFromAll str),
    password = "",
    path = [],
    port' = "",
    protocol = "",
    query = (queryFromAll str),
    username = ""
  }
