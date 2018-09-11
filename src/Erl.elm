module Erl exposing
    ( Url
    , parse
    , new
    , toString, toAbsoluteString
    )

{-| Library for parsing and constructing URLs


# Types

@docs Url


# Parse

@docs parse


# Construct

@docs new


# Serialize

@docs toString, toAbsoluteString

-}

import Char
import Erl.Query exposing (Query)
import Http
import Parser exposing (..)
import Regex
import String
import Url



-- TYPES


{-| Record that holds url attributes
-}
type alias Url =
    { protocol : String
    , host : String
    , port_ : Maybe Int
    , pathname : String
    , query : Query
    , hash : String
    }


{-| Parse an url into a Url record

    Erl.parse "http://hello.com/users/1?k=1&q=2#a/b"

    Ok
        { protocol = "http",
        , host = "hello.com",
        , port_ = Just 2000,
        , pathname = "/users/1",
        , query = [ ("k", "1"), ("q", "2") ]
        , hash = "a/b",
        }

-}
parse : String -> Result String Url
parse input =
    run parser input
        |> Result.mapError deadEndsToString


parser : Parser Url
parser =
    succeed Url
        |= protocolParser
        |= hostParser
        |= portParser
        |= pathnameParser
        |= Erl.Query.parser
        |= hashParser
        |. end



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
    Url.percentEncode url.host


portToString : Url -> String
portToString url =
    case url.port_ of
        Nothing ->
            ""

        Just 80 ->
            ""

        Just 443 ->
            if url.protocol == "https" then
                ""

            else
                ":443"

        Just other ->
            ":" ++ String.fromInt other


pathnameToString : Url -> String
pathnameToString url =
    let
        encoded =
            url.pathname
                |> Url.percentEncode
                |> decodeSymbol "/"

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


{-| @priv
Decode one symbol in a string


# decodeSymbol ">" "hello%3Eworld"

"hello>world"

-}
decodeSymbol : String -> String -> String
decodeSymbol symbol =
    let
        encoded =
            Url.percentEncode symbol

        regex : Regex.Regex
        regex =
            Regex.fromString encoded
                |> Maybe.withDefault Regex.never
    in
    Regex.replace regex (\_ -> symbol)


{-| Convert to a string the hash component of an url, this includes '#'

    hashToString url == "#a/b"

-}
hashToString : Url -> String
hashToString url =
    if String.isEmpty url.hash then
        ""

    else if String.startsWith "#" url.hash then
        url.hash

    else
        "#" ++ url.hash


{-| Convert to a string the query component of an url, this includes '?'

    queryToString url == "?k=1"

-}
queryToString : Url -> String
queryToString url =
    let
        encoded =
            url.query
                |> List.map (\( k, v ) -> Url.percentEncode k ++ "=" ++ Url.percentEncode v)
                |> String.join "&"
    in
    if List.isEmpty url.query then
        ""

    else
        "?" ++ encoded


{-| Generate an empty Url record

    Erl.new
        == { protocol = ""
           , host = ""
           , port_ = Nothing
           , pathname = ""
           , query = []
           , hash = ""
           }

-}
new : Url
new =
    { protocol = ""
    , host = ""
    , pathname = ""
    , port_ = Nothing
    , query = []
    , hash = ""
    }


{-| Generate a url string from an Url record

    url = { protocol = "http",
          , host = "www.hello.com",
          , port_ = Just 2000,
          , pathname = "/users/1",
          , query = [ ("k", "1"), ("q", "2") ]
          , hash = "a/b",
          }

    Erl.toString url == "http://www.hello.com:2000/users/1?k=1&q=2#a/b"

-}
toString : Url -> String
toString url =
    protocolToString url
        ++ hostToString url
        ++ portToString url
        ++ toAbsoluteString url


{-| Generate a url that starts at the path

    url = { protocol = "http",
          , host = "www.hello.com",
          , port_ = Just 2000,
          , pathname = "/users/1",
          , query = [ ("k", "1"), ("q", "2") ]
          , hash = "a/b",
          }

    Erl.toAbsoluteString url == "/users/1?k=1&q=2#a/b"

-}
toAbsoluteString : Url -> String
toAbsoluteString url =
    pathnameToString url
        ++ queryToString url
        ++ hashToString url



-- NEW


protocolParser : Parser String
protocolParser =
    oneOf
        [ protocolPresentParser
        , succeed ""
        ]


protocolPresentParser : Parser String
protocolPresentParser =
    getChompedString <|
        succeed identity
            |= chompIf Char.isLower
            |. keyword "://"


hostParser : Parser String
hostParser =
    oneOf
        [ hostPresentParser
        , succeed ""
        ]


hostPresentParser : Parser String
hostPresentParser =
    getChompedString <| chompIf (\c -> c /= ':' && c /= '/' && c /= '?')


portParser : Parser (Maybe Int)
portParser =
    oneOf
        [ Parser.map Just <| succeed identity |. symbol ":" |= int
        , succeed Nothing
        ]


pathnameParser : Parser String
pathnameParser =
    getChompedString <| chompIf (\c -> c /= '#' && c /= '?')


hashParser : Parser String
hashParser =
    oneOf
        [ hashPresentParser
        , succeed ""
        ]


hashPresentParser : Parser String
hashPresentParser =
    succeed identity
        |. symbol "#"
        |= (getChompedString <| chompIf (always True))
        |. end
