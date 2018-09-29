module ErlTests exposing (all)

import Dict
import Erl
import Expect
import String
import Test exposing (..)



-- TO STRING


url : Erl.Url
url =
    { protocol = "http"
    , host = "www.hello.com"
    , port_ = Just 2000
    , pathname = "/users/1"
    , query = [ ( "k", "1" ) ]
    , hash = "a/b"
    }


toStringTest testCase input expected =
    test testCase <|
        \_ ->
            Expect.equal (Erl.toString input) expected


toStringTests =
    describe "toString"
        [ toStringTest
            "it converts to string"
            url
            "http://www.hello.com:2000/users/1?k=1#a/b"
        , toStringTest
            "it can have a trailing slash"
            { url | pathname = "/users/1/" }
            "http://www.hello.com:2000/users/1/?k=1#a/b"
        , toStringTest
            "it can be missing the leading /"
            { url | pathname = "users/1" }
            "http://www.hello.com:2000/users/1?k=1#a/b"
        , toStringTest
            "it can have an empty protocol"
            { url | protocol = "" }
            "www.hello.com:2000/users/1?k=1#a/b"
        , toStringTest
            "it doesn't include the port when it is 80"
            { url | port_ = Just 80 }
            "http://www.hello.com/users/1?k=1#a/b"
        , toStringTest
            "it doesn't include the port when it is 443 and the protocol is https"
            { url | protocol = "https", port_ = Just 443 }
            "https://www.hello.com/users/1?k=1#a/b"
        , toStringTest
            "it doesn't add # when hash is empty"
            { url | hash = "" }
            "http://www.hello.com:2000/users/1?k=1"
        , toStringTest "it doesn't add query when query is empty"
            { url | query = [] }
            "http://www.hello.com:2000/users/1#a/b"
        , --
          toStringTest
            "it encodes values in host"
            { url | host = "aa/bb" }
            "http://aa%2Fbb:2000/users/1?k=1#a/b"
        , toStringTest
            "it encodes values in path"
            { url | pathname = "aa/b&b" }
            "http://www.hello.com:2000/aa/b%26b?k=1#a/b"
        , toStringTest
            "it encodes values in query"
            { url | query = [ ( "a/b", "c/d" ) ] }
            "http://www.hello.com:2000/users/1?a%2Fb=c%2Fd#a/b"
        , toStringTest
            "it handles localhost which has no ."
            { url | host = "localhost" }
            "http://localhost:2000/users/1?k=1#a/b"
        ]


roundTripTest testCase input =
    test testCase <|
        \_ ->
            Expect.equal (input |> Erl.parse |> Result.withDefault Erl.new |> Erl.toString) input


roundTripTests =
    describe "Round trip"
        [ roundTripTest "Just host" "http://example.com"
        , roundTripTest "Host with port" "http://example.com:2000"
        , roundTripTest "With path" "http://example.com/users"
        , roundTripTest "Path with trailing slash" "http://example.com/users/"
        , roundTripTest "Deeper path" "http://example.com/users/1"
        , roundTripTest "With query string" "http://example.com/users/1?color=red"
        , roundTripTest "With hash" "http://example.com/users/1#a/b"
        , roundTripTest "With query and hash" "http://example.com/users/1?color=red#a/b"
        ]


parseTest testCase input expected =
    test (testCase ++ " - " ++ input) <|
        \_ ->
            Expect.equal (Erl.parse input) expected


parseTests =
    describe "parse"
        [ parseTest
            "it works"
            "http://hello.com:3000"
            (Ok
                { protocol = "http"
                , host = "hello.com"
                , port_ = Just 3000
                , pathname = ""
                , query = []
                , hash = ""
                }
            )

        -- protocol
        , parseTest
            "it may have a protocol"
            "https://hello.com"
            (Ok
                { protocol = "https"
                , host = "hello.com"
                , port_ = Nothing
                , pathname = ""
                , query = []
                , hash = ""
                }
            )

        -- port
        , parseTest
            "it can have a port"
            "http://hello.com:3000"
            (Ok
                { protocol = "http"
                , host = "hello.com"
                , port_ = Just 3000
                , pathname = ""
                , query = []
                , hash = ""
                }
            )
        , parseTest
            "it can have a port before the path"
            "http://hello.com:3000/a"
            (Ok
                { protocol = "http"
                , host = "hello.com"
                , port_ = Just 3000
                , pathname = "/a"
                , query = []
                , hash = ""
                }
            )
        , parseTest
            "it may not have a port"
            "http://hello.com"
            (Ok
                { protocol = "http"
                , host = "hello.com"
                , port_ = Nothing
                , pathname = ""
                , query = []
                , hash = ""
                }
            )

        -- Path
        , parseTest
            "it can have a pathname"
            "http://hello.com/a/b/c"
            (Ok
                { protocol = "http"
                , host = "hello.com"
                , port_ = Nothing
                , pathname = "/a/b/c"
                , query = []
                , hash = ""
                }
            )
        , parseTest
            "it can have a pathname with extension"
            "http://hello.com/a/b/c.html"
            (Ok
                { protocol = "http"
                , host = "hello.com"
                , port_ = Nothing
                , pathname = "/a/b/c.html"
                , query = []
                , hash = ""
                }
            )
        , parseTest
            "it can get the file pathname"
            "file:///foo/bar"
            (Ok
                { protocol = "file"
                , host = ""
                , port_ = Nothing
                , pathname = "/foo/bar"
                , query = []
                , hash = ""
                }
            )
        , parseTest
            "it can have a trailing slash"
            "http://hello.com/a/b/"
            (Ok
                { protocol = "http"
                , host = "hello.com"
                , port_ = Nothing
                , pathname = "/a/b/"
                , query = []
                , hash = ""
                }
            )

        -- Query
        , parseTest
            "it can have a query"
            "http://hello.com/a?a=1&b=2"
            (Ok
                { protocol = "http"
                , host = "hello.com"
                , port_ = Nothing
                , pathname = "/a"
                , query = [ ( "a", "1" ), ( "b", "2" ) ]
                , hash = ""
                }
            )
        , parseTest
            "query without path"
            "http://hello.com?a=1&b=2"
            (Ok
                { protocol = "http"
                , host = "hello.com"
                , port_ = Nothing
                , pathname = ""
                , query = [ ( "a", "1" ), ( "b", "2" ) ]
                , hash = ""
                }
            )
        , parseTest
            "query without path with trailing /"
            "http://hello.com/?a=1&b=2"
            (Ok
                { protocol = "http"
                , host = "hello.com"
                , port_ = Nothing
                , pathname = "/"
                , query = [ ( "a", "1" ), ( "b", "2" ) ]
                , hash = ""
                }
            )

        -- Hash
        , parseTest
            "it can have a hash"
            "http://hello.com/a?a=1#x=1"
            (Ok
                { protocol = "http"
                , host = "hello.com"
                , port_ = Nothing
                , pathname = "/a"
                , query = [ ( "a", "1" ) ]
                , hash = "x=1"
                }
            )
        , parseTest
            "it can have a hash without query"
            "http://hello.com/a#x=1"
            (Ok
                { protocol = "http"
                , host = "hello.com"
                , port_ = Nothing
                , pathname = "/a"
                , query = []
                , hash = "x=1"
                }
            )
        , parseTest
            "it can have a missing host"
            "/a#x=1"
            (Ok
                { protocol = ""
                , host = ""
                , port_ = Nothing
                , pathname = "/a"
                , query = []
                , hash = "x=1"
                }
            )
        ]


all : Test
all =
    describe "Erl Tests"
        [ roundTripTests
        , toStringTests
        , parseTests
        ]
