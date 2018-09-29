module Erl.Parsers exposing (protocolParser)

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
