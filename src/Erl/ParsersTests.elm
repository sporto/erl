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
    describe "protocolParser"
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



-- Host


hostParserTest testCase input expected =
    test testCase <|
        \_ ->
            Expect.equal
                (run hostParser input |> discardError)
                expected


hostParserTests =
    describe "hostParser"
        [ hostParserTest
            "It parses"
            "example.com"
            (Ok "example.com")
        ]



-- Port


portParserTest testCase input expected =
    test testCase <|
        \_ ->
            Expect.equal
                (run portParser input |> discardError)
                expected


portParserTests =
    describe "portParser"
        [ portParserTest
            "It parses"
            ":3000"
            (Ok (Just 3000))
        , portParserTest
            "It fails without :"
            "3000"
            (Ok Nothing)
        ]



-- Pathname


pathnameParserTest testCase input expected =
    test testCase <|
        \_ ->
            Expect.equal
                (run pathnameParser input |> discardError)
                expected


pathnameParserTests =
    describe "pathnameParser"
        [ pathnameParserTest
            "It parses"
            "/hello/world"
            (Ok "/hello/world")
        ]



-- Hash


hashParserTest testCase input expected =
    test testCase <|
        \_ ->
            Expect.equal
                (run hashParser input |> discardError)
                expected


hashParserTests =
    describe "hashParser"
        [ hashParserTest
            "It parses"
            "#the-end"
            (Ok "the-end")
        ]


all : Test
all =
    describe "Parsers Tests"
        [ protocolParserTests
        , hostParserTests
        , portParserTests
        , pathnameParserTests
        , hashParserTests
        ]
