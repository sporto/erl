module Erl.Query
    exposing
        ( toString
        , add
        , set
        , remove
        , getValuesForKey
        )

{-| Library for parsing and constructing URLs

# Mutation helpers

@docs add, set, remove

# Serialize

@docs toString

# Other helpers

@docs getValuesForKey
-}

import Erl.Types as Types
import Http
import String


{-| Convert to a string only the query component of an url, this includes '?'

    Erl.Query.toString url.query == "?a=1&b=2"

-}
toString : Types.Query -> String
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
            "?" ++ (String.join "&" parts)


{-| Adds key/value in query string

    Erl.Query.add key value query

This doesn't replace existing keys, so if this is a duplicated this key is just added.

-}
add : String -> String -> Types.Query -> Types.Query
add key val =
    List.reverse
        >> (::) ( key, val )
        >> List.reverse


{-| Set key/value in query string, removes any existing one if necessary.

    Erl.Query.set key value query

-}
set : String -> String -> Types.Query -> Types.Query
set key val query =
    let
        without =
            remove key query
    in
        add key val without


{-| Removes key from query string

    Erl.Query.remove key query

-}
remove : String -> Types.Query -> Types.Query
remove key query =
    List.filter (\( k, v ) -> k /= key) query


{-| Gets values for a key in the query

    url = Erl.parse "?a=1&b=2&a=3"

    Erl.Query.getQueryValuesForKey "a" url.query

    == ["1", "3"]

-}
getValuesForKey : String -> Types.Query -> List String
getValuesForKey key =
    List.filter (\( k, _ ) -> k == key)
        >> List.map Tuple.second
