module Erl.Parsers exposing (hashParser, hostParser, pathnameParser, portParser, protocolParser)

import Parser exposing (..)


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
            |. chompIf Char.isLower
            |. chompWhile Char.isLower
            |. chompUntil "://"


hostParser : Parser String
hostParser =
    oneOf
        [ hostPresentParser
        , succeed ""
        ]


hostPresentParser : Parser String
hostPresentParser =
    getChompedString <|
        chompWhile (\c -> c /= ':' && c /= '/' && c /= '?')


portParser : Parser (Maybe Int)
portParser =
    oneOf
        [ Parser.map Just <| portParserPreset
        , succeed Nothing
        ]


portParserPreset : Parser Int
portParserPreset =
    succeed identity
        |. symbol ":"
        |= int


pathnameParser : Parser String
pathnameParser =
    getChompedString <| chompWhile (\c -> c /= '#' && c /= '?')


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
        |= (getChompedString <| chompWhile (always True))
        |. end
