module Erl (
  extractFragment,
  extractHost,
  extractPath, 
  extractPort,
  extractProtocol, 
  extractQuery,
  parse,
  toString,
  Url
  ) where

{-| Library for parsing and constructing URLs

# Definition
@docs Url

# Common Helpers
@docs extractFragment, extractHost, extractPath, extractProtocol, extractPort, extractQuery, parse, toString
-}

import Dict
import String exposing (..)
import Regex
import Debug

-- TYPES

type alias Host = List String
type alias Fragment = List String
type alias Query = Dict.Dict String String

{-| Url record that holds url attributes

-}
type alias Url = {
  host: Host,
  fragment: Fragment,
  password: String,
  path: List String,
  port': Int,
  protocol: String,
  query: Query,
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

parseHost: String -> Host
parseHost str =
  split "." str

host: String -> Host
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

parseQuery: String -> Dict.Dict String String
parseQuery str =
  Dict.fromList (queryTuples str)

queryFromAll: String -> Dict.Dict String String
queryFromAll all =
  all
    |> extractQuery
    |> parseQuery

{-| Parse a Url an return a Erl.Url record

    parse "http://api.example.com/users/1#x/1?a=1" == Erl.Url{...}
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

{-| Generate url string from an Erl.Url record
-}
toString: Url -> String
toString url =
  ""
