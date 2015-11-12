module Erl where

import Dict
import String exposing (..)
import Regex
import Debug

type alias Url = {
  host: String,
  fragment: List String,
  password: String,
  path: List String,
  port': Int,
  protocol: String,
  query: Dict.Dict String String,
  username: String
}

-- UTILS

notEmpty: String -> Bool
notEmpty str = 
  not (isEmpty str)

-- "aa#bb" --> "bb"
rightFrom: String -> String -> String
rightFrom delimiter str =
  let
    parts =
      split delimiter str
  in
    case List.length parts of
      0 ->
        ""
      1 ->
        ""
      _ ->
        parts
          |> List.reverse
          |> List.head
          |> Maybe.withDefault ""

rightFromOrSame: String -> String -> String
rightFromOrSame delimiter str =
  let
    parts =
      split delimiter str
  in
    parts
      |> List.reverse
      |> List.head
      |> Maybe.withDefault ""


  --let
  --  ixs =
  --    indexes delimiter str
  --in
  --  case ixs of
  --    [] ->
  --      ""
  --    _ ->
  --      ""

  --let
  --  rx =
  --    Regex.regex ("(?<=\#).+")
  --in
  --  str
  --    |> Regex.find (Regex.AtMost 1) rx
  --    |> List.map .match
  --    |> List.head
  --    |> Maybe.withDefault ""

leftFrom: String -> String -> String
leftFrom delimiter str =
  let
    parts =
      split delimiter str
  in
    parts
      |> List.head
      |> Maybe.withDefault ""

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

-- HOST

extractHost: String -> String
extractHost str =
  str
    |> rightFromOrSame "://"
    |> leftFrom "/"
    |> Regex.find (Regex.AtMost 1) (Regex.regex "(\\w+\\.)+\\w+")
    |> List.map .match
    |> List.head
    |> Maybe.withDefault ""

-- PORT

extractPort: String -> Int
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
      |> toInt
      |> Result.toMaybe
      |> Maybe.withDefault 80

-- PATH

extractPath: String -> String
extractPath str =
  let
    host =
      extractHost str
  in
    str
      |> rightFromOrSame "://"
      |> leftFrom "?"
      |> leftFrom "#"
      |> Regex.replace Regex.All (Regex.regex host) (\_ -> "")

parsePath: String -> List String
parsePath str =
  let
    parts =
      split "/" str
  in
    List.filter notEmpty parts

pathFromAll: String -> List String
pathFromAll str =
  parsePath (extractPath str)

-- FRAGMENT

extractFragment: String -> String
extractFragment str =
  let
    parts = split "#" str
    maybeFirst = List.head (List.drop 1 parts)
  in
    Maybe.withDefault "" maybeFirst

parseFragment: String -> List String
parseFragment str =
  let
    parts = 
      split "/" str
  in
    List.filter notEmpty parts

fragmentFromAll: String -> List String
fragmentFromAll str =
  parseFragment (extractFragment str)

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
    host = (extractHost str),
    fragment = (fragmentFromAll str),
    password = "",
    path = (pathFromAll str),
    port' = (extractPort str),
    protocol = "",
    query = (queryFromAll str),
    username = ""
  }
