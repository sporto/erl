module Erl.Query exposing
    ( Query
    , parse, parser
    , add, set, remove
    , toString
    , getValuesForKey
    )

{-| Functions to work with a Query record


# Types

@docs Query


# Parse

@docs parse, parser


# Mutation helpers

@docs add, set, remove


# Serialize

@docs toString


# Other helpers

@docs getValuesForKey

-}

import Http
import Parser exposing (..)
import String


{-| List holding query string values
-}
type alias Query =
    List ( String, String )


{-| Parse a query string

    Erl.Query.parse "?a=1&b=2&a=3" == [ ( "a", "1" ), ( "b", "2" ), ( "a", "1" ) ]

-}
parse : String -> Result Parser.Error Query
parse input =
    run parser input


{-| Query Parser
-}
parser : Parser Query
parser =
    oneOf
        [ succeed identity
            |. symbol "?"
            |= repeat oneOrMore kvParser
        , succeed []
        ]


kvParser : Parser ( String, String )
kvParser =
    succeed (\a b -> ( a, b ))
        |= map decodeUri keyParser
        |. symbol "="
        |= map decodeUri valueParser
        |. oneOf
            [ symbol "&"
            , succeed ()
            ]


keyParser : Parser String
keyParser =
    keep oneOrMore (\c -> c /= '=' && c /= '#')


valueParser : Parser String
valueParser =
    keep oneOrMore (\c -> c /= '&' && c /= '#')


decodeUri : String -> String
decodeUri =
    Http.decodeUri >> Maybe.withDefault ""


{-| Convert to a string, this includes '?'

    Erl.Query.toString query == "?a=1&b=2"

-}
toString : Query -> String
toString query =
    let
        encodedTuples =
            List.map (\( x, y ) -> ( Http.encodeUri x, Http.encodeUri y )) query

        parts =
            List.map (\( a, b ) -> a ++ "=" ++ b) encodedTuples
    in
    if List.isEmpty query then
        ""

    else
        "?" ++ String.join "&" parts


{-| Adds key/value in query string

    Erl.Query.add key value query

This doesn't replace existing keys, so if this is a duplicated this key is just added.

-}
add : String -> String -> Query -> Query
add key val =
    List.reverse
        >> (::) ( key, val )
        >> List.reverse


{-| Set key/value in query string, removes any existing one if necessary.

    Erl.Query.set key value query

-}
set : String -> String -> Query -> Query
set key val query =
    let
        without =
            remove key query
    in
    add key val without


{-| Removes key from query string

    Erl.Query.remove key query

-}
remove : String -> Query -> Query
remove key query =
    List.filter (\( k, v ) -> k /= key) query


{-| Gets values for a key in the query

    url = Erl.parse "?a=1&b=2&a=3"

    Erl.Query.getQueryValuesForKey "a" url.query

    == ["1", "3"]

-}
getValuesForKey : String -> Query -> List String
getValuesForKey key =
    List.filter (\( k, _ ) -> k == key)
        >> List.map Tuple.second
