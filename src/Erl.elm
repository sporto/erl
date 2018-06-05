module Erl
    exposing
        ( new
        , parse
        , queryToString
        , toString
        , toAbsoluteString
        , Url
        )

{-| Library for parsing and constructing URLs


# Types

@docs Url


# Parse

@docs parse


# Parse helpers

@docs extractHash, extractHost, extractPath, extractProtocol, extractPort, extractQuery


# Construct

@docs new


# Mutation helpers

@docs addQuery, setQuery, removeQuery, clearQuery, appendPathSegments


# Serialize

@docs toString, toAbsoluteString


# Serialization helpers

@docs queryToString


# Other helpers

@docs getQueryValuesForKey

-}

import Erl.Query
import Http
import Parser exposing (..)
import Char
import String exposing (..)


-- TYPES


{-| Record that holds url attributes
-}
type alias Url =
    { protocol : String
    , host : String
    , port_ : Maybe Int
    , pathname : String
    , query : String
    , hash : String
    }



-- TO STRING


protocolToString : Url -> String
protocolToString url =
    case url.protocol of
        "" ->
            ""

        _ ->
            url.protocol ++ "://"


hostToString : Url -> String
hostToString url =
    Http.encodeUri url.host


portToString : Url -> String
portToString url =
    case url.port_ of
        Nothing ->
            ""

        Just 80 ->
            ""

        Just other ->
            ":" ++ (Basics.toString other)


pathnameToString : Url -> String
pathnameToString url =
    let
        encoded =
            Http.encodeUri url.pathname

        leadingSlash =
            if String.startsWith "/" url.pathname then
                ""
            else
                "/"
    in
        if String.isEmpty url.pathname then
            ""
        else
            leadingSlash ++ encoded


{-| Convert to a string the hash component of an url, this includes '#'

    hashToString url == "#a/b"

-}
hashToString : Url -> String
hashToString url =
    if String.isEmpty url.hash then
        ""
    else
        "#" ++ url.hash


{-| Convert to a string the query component of an url, this includes '?'

    queryToString url == "?k=1"

-}
queryToString : Url -> String
queryToString url =
    if String.isEmpty url.query then
        ""
    else if String.startsWith "?" url.query then
        url.query
    else
        "?" ++ url.query


{-| Generate an empty Url record

    Erl.new ==

    { protocol = ""
    , host = ""
    , pathname = ""
    , port_ = Nothing
    , hash = ""
    , query = ""
    }

-}
new : Url
new =
    { protocol = ""
    , host = ""
    , pathname = ""
    , port_ = Nothing
    , hash = ""
    , query = ""
    }


{-| Generate a url string from an Url record

    url = { protocol = "http",
          , host = "www.hello.com",
          , pathname = "/users/1",
          , port_ = 2000,
          , hash = "a/b",
          , query = "k=1&q=2"
          }

    Erl.toString url == "http://www.hello.com:2000/users/1?k=1&q=2#a/b"

-}
toString : Url -> String
toString url =
    (protocolToString url)
        ++ (hostToString url)
        ++ (portToString url)
        ++ (toAbsoluteString url)


{-| Generate a url that starts at the path

    url = { protocol = "http",
          , host = "www.hello.com",
          , pathname = "/users/1",
          , port_ = 2000,
          , hash = "a/b",
          , query = "k=1&q=2"
          }

    Erl.toAbsoluteString url == "/users/1?k=1&q=2#a/b"

-}
toAbsoluteString : Url -> String
toAbsoluteString url =
    (pathnameToString url)
        ++ (queryToString url)
        ++ (hashToString url)



-- NEW


protocolParser : Parser String
protocolParser =
    map2
        (\prot _ -> prot)
        (keep oneOrMore Char.isLower)
        (keyword "://")


hostParser : Parser String
hostParser =
    oneOf
        [ keep oneOrMore (\c -> c /= ':' && c /= '/')
        , succeed ""
        ]


portParser : Parser (Maybe Int)
portParser =
    oneOf
        [ Parser.map Just <| succeed identity |. keyword ":" |= int
        , succeed Nothing
        ]


pathnameParser : Parser String
pathnameParser =
    keep zeroOrMore (\c -> c /= '#' && c /= '?')


queryParser : Parser String
queryParser =
    oneOf
        [ succeed identity
            |. keyword "?"
            |= (keep zeroOrMore (\c -> c /= '#'))
        , succeed ""
        ]


hashParser : Parser String
hashParser =
    oneOf
        [ succeed identity
            |. keyword "#"
            |= (keep oneOrMore (always True))
            |. end
        , succeed ""
        ]


parser : Parser Url
parser =
    succeed Url
        |= protocolParser
        |= hostParser
        |= portParser
        |= pathnameParser
        |= queryParser
        |= hashParser


parse input =
    run parser input
