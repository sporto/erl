module Erl.ParsersTests exposing (all)

import Erl.Parsers exposing (..)
import Expect
import Parser exposing (run)
import Test exposing (..)


discardError =
    Result.mapError (always ())


protocolParserTest testCase input expected =
    test testCase <|
        \_ ->
            Expect.equal
                (run protocolParser input |> discardError)
                expected


protocolParserTests =
    describe "protocolParserTests"
        [ protocolParserTest
            "It parsers"
            "http://"
            (Ok "http")
        , protocolParserTest
            "It parsers nothing"
            ""
            (Ok "")
        , protocolParserTest
            "It fails"
            "http"
            (Err ())
        ]


all : Test
all =
    describe "Parsers Tests"
        [ protocolParserTests ]
        |> only
