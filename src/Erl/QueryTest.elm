module Erl.QueryTest exposing (..)


testAddQuery =
    let
        inputs =
            [ ( "1"
              , Erl.new
                    |> Erl.addQuery "a" "1"
                    |> Erl.addQuery "b" "2"
                    |> .query
              , [ ( "a", "1" ), ( "b", "2" ) ]
              )
            , ( "2"
              , Erl.new
                    |> Erl.addQuery "a" "1"
                    |> Erl.addQuery "a" "2"
                    |> .query
              , [ ( "a", "1" ), ( "a", "2" ) ]
              )
            ]

        run ( name, actual, expected ) =
            test ("addQuery " ++ name) <|
                \() -> Expect.equal expected actual
    in
        describe "Adds to the query"
            (List.map run inputs)


testSetQuery =
    let
        inputs =
            [ ( Erl.new
                    |> Erl.addQuery "a" "1"
                    |> Erl.setQuery "a" "2"
                    |> .query
              , [ ( "a", "2" ) ]
              )
            ]

        run ( actual, expected ) =
            test "setQuery" <|
                \() -> Expect.equal expected actual
    in
        describe "Sets the query"
            (List.map run inputs)


testRemoveQuery =
    let
        expected =
            [ ( "a", "1" ) ]

        actual =
            Erl.new
                |> Erl.addQuery "a" "1"
                |> Erl.addQuery "b" "2"
                |> Erl.removeQuery "b"
                |> .query
    in
        test "Removes the query" <|
            \() -> Expect.equal expected actual


testQueryClear =
    let
        expected =
            []

        actual =
            Erl.new
                |> Erl.setQuery "a" "1"
                |> Erl.clearQuery
                |> .query
    in
        test "Cleans the query" <|
            \() -> Expect.equal expected actual


testGetQueryValuesForKey =
    let
        url =
            Erl.parse "?a=1&b=2&a=3"

        input =
            [ ( "a", [ "1", "3" ] )
            , ( "c", [] )
            ]

        run ( key, expected ) =
            let
                actual =
                    Erl.getQueryValuesForKey key url
            in
                test ("getQueryValuesForKey " ++ key) <|
                    \() -> Expect.equal expected actual
    in
        describe "Gets query values" <| List.map run input


testToString =
    let
        url1 =
            { protocol = "http"
            , username = ""
            , password = ""
            , host = [ "www", "foo", "com" ]
            , path = [ "users", "1" ]
            , hasLeadingSlash = True
            , hasTrailingSlash = False
            , port_ = 2000
            , hash = "a/b"
            , query = [ ( "q", "1" ), ( "k", "2" ) ]
            }

        url2 =
            { protocol = ""
            , username = ""
            , password = ""
            , host = []
            , port_ = 0
            , path = []
            , hasLeadingSlash = False
            , hasTrailingSlash = False
            , hash = ""
            , query = []
            }

        inputs =
            [ ( "it converts to string"
              , url1
              , "http://www.foo.com:2000/users/1?q=1&k=2#a/b"
              )
            , ( "it can have a trailing slash"
              , { url1 | hasTrailingSlash = True }
              , "http://www.foo.com:2000/users/1/?q=1&k=2#a/b"
              )
            , ( "it can have an empty protocol"
              , { url1 | protocol = "" }
              , "www.foo.com:2000/users/1?q=1&k=2#a/b"
              )
            , ( "it doesn't include the port when it is 80"
              , { url1 | port_ = 80 }
              , "http://www.foo.com/users/1?q=1&k=2#a/b"
              )
            , ( "it doesn't add # when hash is empty"
              , { url1 | hash = "" }
              , "http://www.foo.com:2000/users/1?q=1&k=2"
              )
            , ( "it doesn't add query when query is empty"
              , { url1 | query = [] }
              , "http://www.foo.com:2000/users/1#a/b"
              )
            , ( "it adds duplicate values in the query"
              , { url1 | query = [ ( "a", "1" ), ( "a", "2" ) ] }
              , "http://www.foo.com:2000/users/1?a=1&a=2#a/b"
              )
            , --
              ( "it encodes values in host"
              , { url1 | host = [ "aa/bb", "com" ] }
              , "http://aa%2Fbb.com:2000/users/1?q=1&k=2#a/b"
              )
            , ( "it encodes values in path"
              , { url1 | path = [ "aa/bb", "2" ] }
              , "http://www.foo.com:2000/aa%2Fbb/2?q=1&k=2#a/b"
              )
            , ( "it encodes values in query"
              , { url1 | query = [ ( "a/b", "c/d" ) ] }
              , "http://www.foo.com:2000/users/1?a%2Fb=c%2Fd#a/b"
              )
            , ( "it handles localhost which has no ."
              , { url1 | host = [ "localhost" ] }
              , "http://localhost:2000/users/1?q=1&k=2#a/b"
              )
            , ( "it handles a url without host, port, path"
              , { url2 | hash = "a/b", query = [ ( "k", "1" ) ] }
              , "?k=1#a/b"
              )
            , ( "it handles a url with only query"
              , { url2 | query = [ ( "k", "1" ) ] }
              , "?k=1"
              )
            ]

        run ( testCase, input, expected ) =
            let
                actual =
                    Erl.toString input

                result =
                    Expect.equal expected actual
            in
                test testCase <| \() -> result
    in
        describe "toString"
            (List.map run inputs)


all : Test
all =
    describe "Tests"
        [ testAddQuery
        , testGetQueryValuesForKey
        , testQuery
        , testQueryClear
        , testQueryExtract
        , testQueryToString
        , testRemoveQuery
        , testSetQuery
        , testToString
        ]
