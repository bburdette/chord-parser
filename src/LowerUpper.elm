module LowerUpper exposing (Uplow(..), lowers, uplowParser, uplowsParser, uppers)

import ParseHelp exposing (listOf)
import Parser as P exposing ((|.), (|=), Parser)


lowers : Parser String
lowers =
    P.getChompedString <|
        P.chompIf (\c -> Char.isLower c)
            |. P.chompWhile (\c -> Char.isLower c)


uppers : Parser String
uppers =
    P.getChompedString <|
        P.chompIf (\c -> Char.isUpper c)
            |. P.chompWhile (\c -> Char.isUpper c)


others : Parser String
others =
    let
        cond =
            \c -> not (Char.isUpper c || Char.isLower c)
    in
    P.getChompedString <|
        P.chompIf cond
            |. P.chompWhile cond


type Uplow
    = Up String
    | Low String
    | Other String


uplowParser : Parser Uplow
uplowParser =
    P.oneOf
        [ P.succeed Up |= uppers
        , P.succeed Low |= lowers
        , P.succeed Other |= others
        ]


uplowsParser : Parser (List Uplow)
uplowsParser =
    listOf uplowParser
