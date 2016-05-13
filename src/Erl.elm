module Erl exposing (addQuery, clearQuery, extractHash, extractHost, extractPath, extractPort, extractProtocol, extractQuery, new, parse, removeQuery, setQuery, appendPathSegments, toString, queryToString, Url, Query)

{-| Library for parsing and constructing URLs

# Types
@docs Url, Query

# Parse
@docs parse

# Parse helpers
@docs extractHash, extractHost, extractPath, extractProtocol, extractPort, extractQuery

# Construct
@docs new

# Mutation helpers
@docs addQuery, setQuery, removeQuery, clearQuery, appendPathSegments

# Serialize
@docs toString

# Serialization helpers
@docs queryToString

-}

import Dict
import Http
import Regex
import String exposing (..)


-- TYPES


{-| A Dict that holds keys and values for the query string
-}
type alias Query =
  Dict.Dict String String


{-| Record that holds url attributes
-}
type alias Url =
  { protocol : String
  , username : String
  , password : String
  , host : List String
  , port' : Int
  , path : List String
  , hasTrailingSlash : Bool
  , hash : String
  , query : Query
  }



-- UTILS


notEmpty : String -> Bool
notEmpty str =
  not (isEmpty str)



-- "aa#bb" --> "bb"


rightFrom : String -> String -> String
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


rightFromOrSame : String -> String -> String
rightFromOrSame delimiter str =
  let
    parts =
      split delimiter str
  in
    parts
      |> List.reverse
      |> List.head
      |> Maybe.withDefault ""



-- "a/b" -> "a"
-- "a"   => "a"
-- "/b"  => ""


leftFromOrSame : String -> String -> String
leftFromOrSame delimiter str =
  let
    parts =
      split delimiter str
  in
    parts
      |> List.head
      |> Maybe.withDefault ""



-- "a/b" -> "a"
-- "/b"  -> ""
-- "a"   -> ""


leftFrom : String -> String -> String
leftFrom delimiter str =
  let
    parts =
      split delimiter str

    head =
      List.head parts
  in
    case List.length parts of
      0 ->
        ""

      1 ->
        ""

      _ ->
        head |> Maybe.withDefault ""



-- PROTOCOL


{-| Extract the protocol from the url

-}
extractProtocol : String -> String
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



-- valid host: a-z 0-9 and -


extractHost : String -> String
extractHost str =
  let
    dotsRx =
      "((\\w|-)+\\.)+(\\w|-)+"

    localhostRx =
      "localhost"

    rx =
      "(" ++ dotsRx ++ "|" ++ localhostRx ++ ")"
  in
    str
      |> rightFromOrSame "//"
      |> leftFromOrSame "/"
      |> Regex.find (Regex.AtMost 1) (Regex.regex rx)
      |> List.map .match
      |> List.head
      |> Maybe.withDefault ""


parseHost : String -> List String
parseHost str =
  str
    |> split "."


host : String -> List String
host str =
  parseHost (extractHost str)



-- PORT


{-| Extract the port from the url

-}
extractPort : String -> Int
extractPort str =
  let
    rx =
      Regex.regex ":\\d+"

    res =
      Regex.find (Regex.AtMost 1) rx str
  in
    res
      |> List.map .match
      |> List.head
      |> Maybe.withDefault ""
      |> String.dropLeft 1
      |> toInt
      |> Result.toMaybe
      |> Maybe.withDefault 80



-- PATH


{-| Extract the path from the url

-}
extractPath : String -> String
extractPath str =
  let
    host =
      extractHost str
  in
    str
      |> rightFromOrSame "//"
      |> leftFromOrSame "?"
      |> leftFromOrSame "#"
      |> Regex.replace (Regex.AtMost 1) (Regex.regex host) (\_ -> "")
      |> Regex.replace (Regex.AtMost 1) (Regex.regex ":\\d+") (\_ -> "")


parsePath : String -> List String
parsePath str =
  str
    |> split "/"
    |> List.filter notEmpty
    |> List.map Http.uriDecode


pathFromAll : String -> List String
pathFromAll str =
  parsePath (extractPath str)


hasTrailingSlashFromAll : String -> Bool
hasTrailingSlashFromAll str =
  Regex.contains (Regex.regex "/$") (extractPath str)



-- FRAGMENT


{-| Extract the hash (hash) from the url

-}
extractHash : String -> String
extractHash str =
  str
    |> split "#"
    |> List.drop 1
    |> List.head
    |> Maybe.withDefault ""


hashFromAll : String -> String
hashFromAll str =
  extractHash str



-- QUERY


{-| Extract the query string from the url

-}
extractQuery : String -> String
extractQuery str =
  str
    |> split "?"
    |> List.drop 1
    |> List.head
    |> Maybe.withDefault ""
    |> split "#"
    |> List.head
    |> Maybe.withDefault ""



-- "a=1" --> ("a", "1")


queryStringElementToTuple : String -> ( String, String )
queryStringElementToTuple element =
  let
    splitted =
      split "=" element

    first =
      Maybe.withDefault "" (List.head splitted)

    firstDecoded =
      Http.uriDecode first

    second =
      Maybe.withDefault "" (List.head (List.drop 1 splitted))

    secondDecoded =
      Http.uriDecode second
  in
    ( firstDecoded, secondDecoded )



-- "a=1&b=2" --> [("a", "1"), ("b", "2")]


queryTuples : String -> List ( String, String )
queryTuples queryString =
  let
    splitted =
      split "&" queryString
  in
    if String.isEmpty queryString then
      []
    else
      List.map queryStringElementToTuple splitted


parseQuery : String -> Query
parseQuery str =
  Dict.fromList (queryTuples str)


queryFromAll : String -> Query
queryFromAll all =
  all
    |> extractQuery
    |> parseQuery


{-| Parse a url string, returns an Erl.Url record

    Erl.parse "http://api.example.com/users/1#x/1?a=1" == Erl.Url{...}
-}
parse : String -> Url
parse str =
  { host = (host str)
  , hash = (hashFromAll str)
  , password = ""
  , path = (pathFromAll str)
  , hasTrailingSlash = (hasTrailingSlashFromAll str)
  , port' = (extractPort str)
  , protocol = (extractProtocol str)
  , query = (queryFromAll str)
  , username = ""
  }



-- TO STRING


{-| Convert to a string only the query component of an url, this includes '?'

    Erl.queryToString url == "?a=1&b=2"
-}
queryToString : Url -> String
queryToString url =
  let
    tuples =
      Dict.toList url.query

    encodedTuples =
      List.map (\( x, y ) -> ( Http.uriEncode x, Http.uriEncode y )) tuples

    parts =
      List.map (\( a, b ) -> a ++ "=" ++ b) encodedTuples
  in
    if Dict.isEmpty url.query then
      ""
    else
      "?" ++ (join "&" parts)


protocolComponent : Url -> String
protocolComponent url =
  case url.protocol of
    "" ->
      ""

    _ ->
      url.protocol ++ "://"


hostComponent : Url -> String
hostComponent url =
  Http.uriEncode (join "." url.host)


portComponent : Url -> String
portComponent url =
  case url.port' of
    0 ->
      ""

    80 ->
      ""

    _ ->
      ":" ++ (Basics.toString url.port')


pathComponent : Url -> String
pathComponent url =
  let
    encoded =
      List.map Http.uriEncode url.path
  in
    if (List.length url.path) == 0 then
      ""
    else
      "/" ++ (join "/" encoded)


trailingSlashComponent : Url -> String
trailingSlashComponent url =
  if url.hasTrailingSlash == True then
    "/"
  else
    ""


{-| Convert to a string the hash component of an url, this includes '#'

    queryToString url == "#a/b"
-}
hashToString : Url -> String
hashToString url =
  if String.isEmpty url.hash then
    ""
  else
    "#" ++ url.hash


{-| Generate an empty Erl.Url record

    Erl.new ==

    { protocol = ""
    , username = ""
    , password = ""
    , host = []
    , path = []
    , hasTrailingSlash = False
    , port' = 0
    , hash = ""
    , query = Dict.empty
    }

-}
new : Url
new =
  { protocol = ""
  , username = ""
  , password = ""
  , host = []
  , path = []
  , hasTrailingSlash = False
  , port' = 0
  , hash = ""
  , query = Dict.empty
  }


{-| Clears the current query string

    Erl.clearQuery url
-}
clearQuery : Url -> Url
clearQuery url =
  { url | query = Dict.empty }


{-| Set key/value in query string

    Erl.addQuery key value url
-}
addQuery : String -> String -> Url -> Url
addQuery key val url =
  let
    updated =
      if String.isEmpty val then
        Dict.remove key url.query
      else
        Dict.insert key val url.query
  in
    { url | query = updated }


{-| Set key/value in query string, removes any existing ones

    Erl.setQuery key value url
-}
setQuery : String -> String -> Url -> Url
setQuery key val url =
  let
    updated =
      Dict.singleton key val
  in
    { url | query = updated }


{-| Removes key from query string

    Erl.removeQuery key url
-}
removeQuery : String -> Url -> Url
removeQuery key url =
  let
    updated =
      Dict.remove key url.query
  in
    { url | query = updated }


{-| Append some path segments to a url

    Erl.appendPathSegments ["hello", "world"] url
-}
appendPathSegments : List String -> Url -> Url
appendPathSegments segments url =
  let
    newPath =
      List.append url.path segments
  in
    { url | path = newPath }


{-| Generate url string from an Erl.Url record

    url = { protocol = "http",
          , username = "",
          , password = "",
          , host = ["www", "foo", "com"],
          , path = ["users", "1"],
          , hasTrailingSlash = False
          , port' = 2000,
          , hash = "a/b",
          , query = Dict.empty |> Dict.insert "q" "1" |> Dict.insert "k" "2"
          }

    Erl.toString url == "http://www.foo.com:2000/users/1?k=2&q=1#a/b"

-}
toString : Url -> String
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

    trailingSlash' =
      trailingSlashComponent url

    query' =
      queryToString url

    hash =
      hashToString url
  in
    protocol' ++ host' ++ port' ++ path' ++ trailingSlash' ++ query' ++ hash
