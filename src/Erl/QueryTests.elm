module Erl.QueryTests exposing (..)

import Erl.Query as Subject
import Test exposing (..)
import Expect


testParse =
    let
        inputs =
            [ ( "?a=1&b=2", [ ( "a", "1" ), ( "b", "2" ) ] )
            , ( "?a%3F=1%26", [ ( "a?", "1&" ) ] )
            , ( "?a=1&a=2", [ ( "a", "1" ), ( "a", "2" ) ] )
            ]

        run ( input, expected ) =
            test ("Parses the query " ++ input) <|
                \() -> Expect.equal expected (Subject.parse input)
    in
        describe "Query"
            (List.map run inputs)


testAdd =
    let
        inputs =
            [ ( "1"
              , []
                    |> Subject.add "a" "1"
                    |> Subject.add "b" "2"
              , [ ( "a", "1" ), ( "b", "2" ) ]
              )
            , ( "2"
              , []
                    |> Subject.add "a" "1"
                    |> Subject.add "a" "2"
              , [ ( "a", "1" ), ( "a", "2" ) ]
              )
            ]

        run ( name, actual, expected ) =
            test ("add " ++ name) <|
                \() -> Expect.equal expected actual
    in
        describe "Adds to the query"
            (List.map run inputs)


testSet =
    let
        inputs =
            [ ( []
                    |> Subject.add "a" "1"
                    |> Subject.set "a" "2"
              , [ ( "a", "2" ) ]
              )
            ]

        run ( actual, expected ) =
            test "set" <|
                \() -> Expect.equal expected actual
    in
        describe "Sets the query"
            (List.map run inputs)


testRemove =
    let
        expected =
            [ ( "a", "1" ) ]

        actual =
            []
                |> Subject.add "a" "1"
                |> Subject.add "b" "2"
                |> Subject.remove "b"
    in
        test "Removes the query" <|
            \() -> Expect.equal expected actual


testGetQueryValuesForKey =
    let
        query =
            Subject.parse "?a=1&b=2&a=3"

        input =
            [ ( "a", [ "1", "3" ] )
            , ( "c", [] )
            ]

        run ( key, expected ) =
            let
                actual =
                    Subject.getValuesForKey key query
            in
                test ("getValuesForKey " ++ key) <|
                    \() -> Expect.equal expected actual
    in
        describe "Gets query values" <| List.map run input


testToString =
    let
        inputs =
            [ ( "it converts to string"
              , [ ( "q", "1" ), ( "k", "2" ) ]
              , "?q=1&k=2"
              )
            , ( "it doesn't add query when query is empty"
              , []
              , ""
              )
            , ( "it adds duplicate values in the query"
              , [ ( "a", "1" ), ( "a", "2" ) ]
              , "?a=1&a=2"
              )
            , ( "encodes values"
              , [ ( "a/b", "c/d" ) ]
              , "?a%2Fb=c%2Fd"
              )
            ]

        run ( testCase, input, expected ) =
            let
                actual =
                    Subject.toString input

                result =
                    Expect.equal expected actual
            in
                test testCase <| \() -> result
    in
        describe "toString"
            (List.map run inputs)


all : Test
all =
    describe "Query Tests"
        [ testParse
        , testAdd
        , testGetQueryValuesForKey
        , testRemove
        , testSet
        , testToString
        ]
