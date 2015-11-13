module Erl (
  extractFragment,
  extractHost,
  extractPath, 
  extractPort,
  extractProtocol, 
  extractQuery,
  parse,
  toString,
  Url,
  Query
  ) where

{-| Library for parsing and constructing URLs

# Types
@docs Url, Query

# Parse
@docs parse

# Parse helpers
@docs extractFragment, extractHost, extractPath, extractProtocol, extractPort, extractQuery

# Construct
@docs toString

-}

import Dict
import String exposing (..)
import Regex
import Debug

-- TYPES

{-| A Dict that holds keys and values for the query string
-}
type alias Query = Dict.Dict String String

{-| Record that holds url attributes
-}
type alias Url = {
  protocol: String,
  username: String,
  password: String,
  host: List String,
  port': Int,
  path: List String,
  fragment: List String,
  query: Query
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

{-| Extract the protocol from the url

-}
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

{-| Extract the host from the url

-}
extractHost: String -> String
extractHost str =
  str
    |> rightFromOrSame "://"
    |> leftFrom "/"
    |> Regex.find (Regex.AtMost 1) (Regex.regex "(\\w+\\.)+\\w+")
    |> List.map .match
    |> List.head
    |> Maybe.withDefault ""

parseHost: String -> List String
parseHost str =
  split "." str

host: String -> List String
host str =
  parseHost (extractHost str)


-- PORT
{-| Extract the port from the url

-}
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

{-| Extract the path from the url

-}
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

{-| Extract the fragment (hash) from the url

-}
extractFragment: String -> String
extractFragment str =
  str
    |> split "#"
    |> List.drop 1
    |> List.head
    |> Maybe.withDefault ""
    |> split "?"
    |> List.head
    |> Maybe.withDefault ""

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

{-| Extract the query string from the url

-}
extractQuery: String -> String
extractQuery str =
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

parseQuery: String -> Query
parseQuery str =
  Dict.fromList (queryTuples str)

queryFromAll: String -> Query
queryFromAll all =
  all
    |> extractQuery
    |> parseQuery

{-| Parse a url string, returns an Erl.Url record

    Erl.parse "http://api.example.com/users/1#x/1?a=1" == Erl.Url{...}
-}
parse: String -> Url
parse str =
  {
    host = (host str),
    fragment = (fragmentFromAll str),
    password = "",
    path = (pathFromAll str),
    port' = (extractPort str),
    protocol = (extractProtocol str),
    query = (queryFromAll str),
    username = ""
  }

-- TO STRING

queryToString: Query -> String
queryToString query =
  let
    tuples =
      Dict.toList query
    parts =
      List.map (\(a, b) -> a ++ "=" ++ b) tuples
  in
    join "&" parts

protocolComponent: Url -> String
protocolComponent url =
  case url.protocol of
    "" -> ""
    _ ->
      url.protocol ++ "://"

hostComponent: Url -> String
hostComponent url =
  join "." url.host

portComponent: Url -> String
portComponent url =
 case url.port' of
    0 -> 
      ""
    80 ->
      ""
    _ ->
      ":" ++ (Basics.toString url.port')

pathComponent: Url -> String
pathComponent url =
  "/" ++ (join "/" url.path)

fragmentComponent: Url -> String
fragmentComponent url =
  case url.fragment of
    [] -> ""
    _ ->
      "#" ++ (join "/" url.fragment)

queryComponent: Url -> String
queryComponent url =
  case Dict.isEmpty url.query of
    True -> ""
    False ->
      "?" ++ (queryToString url.query)

{-| Generate url string from an Erl.Url record

    url = {
      protocol = "http",
      username = "",
      password = "",
      host = ["www", "foo", "com"],
      path = ["users", "1"],
      port' = 2000,
      fragment = ["a", "b"],
      query = Dict.empty |> Dict.insert "q" "1" |> Dict.insert "k" "2"
    }

    Erl.toString url == "http://www.foo.com:2000/users/1#a/b?k=2&q=1"

-}
toString: Url -> String
toString url =
  let
    protocol' =
      protocolComponent url
    host' =
      hostComponent url
    port' =
      portComponent url
    path' =
      pathComponent url
    fragment' =
      fragmentComponent url
    query' =
      queryComponent url
  in
    protocol' ++ host' ++ port' ++ path' ++ fragment' ++ query'
