module ChordParser exposing (Accidental(..), ChordParse, Note, PitchClass(..), Quality(..), accidentalParser, chordParser, chordsParser, cpHelp, mbSn, mbap, noteParser, pitchParser, qualityParser, slashNoteParser)

import ParseHelp exposing (listOf)
import Parser as P exposing ((|.), (|=), Parser)



--  PitchClass + Maybe Accidental + Qualities + Maybe Slash-note
--  ex. C#M7♭9sus4


type PitchClass
    = PcA
    | PcB
    | PcC
    | PcD
    | PcE
    | PcF
    | PcG


type Quality
    = Major
    | Minor
    | Diminished
    | Augmented
    | Dominant
    | Two
    | Four
    | Five
    | Six
    | Seven
    | Nine
    | Eleven
    | Thirteen
    | Add
    | Suspended
    | QSharp
    | QFlat


type Accidental
    = Sharp
    | Flat


type alias Note =
    { pitchClass : PitchClass
    , accidental : Maybe Accidental
    }


type alias ChordParse =
    { rootNote : Note
    , qualities : List Quality
    , slashNote : Maybe Note
    }


pitchParser : Parser PitchClass
pitchParser =
    P.oneOf
        [ P.succeed PcA |. P.symbol "A"
        , P.succeed PcB |. P.symbol "B"
        , P.succeed PcC |. P.symbol "C"
        , P.succeed PcD |. P.symbol "D"
        , P.succeed PcE |. P.symbol "E"
        , P.succeed PcF |. P.symbol "F"
        , P.succeed PcG |. P.symbol "G"
        ]


qualityParser : Parser Quality
qualityParser =
    P.oneOf
        [ P.succeed Major |. P.symbol "M"
        , P.succeed Minor |. P.symbol "m"
        , P.succeed Diminished |. P.symbol "dim"
        , P.succeed Augmented |. P.symbol "aug"
        , P.succeed Dominant |. P.symbol "dom"
        , P.succeed Two |. P.symbol "2"
        , P.succeed Four |. P.symbol "4"
        , P.succeed Five |. P.symbol "5"
        , P.succeed Six |. P.symbol "6"
        , P.succeed Seven |. P.symbol "7"
        , P.succeed Nine |. P.symbol "9"
        , P.succeed Eleven |. P.symbol "11"
        , P.succeed Thirteen |. P.symbol "13"
        , P.succeed Add |. P.symbol "+"
        , P.succeed Suspended |. P.symbol "sus"
        , P.succeed QSharp |. P.symbol "#"
        , P.succeed QFlat |. P.symbol "♭"
        ]


accidentalParser : Parser Accidental
accidentalParser =
    P.oneOf
        [ P.succeed Sharp |. P.symbol "#"
        , P.succeed Flat |. P.symbol "♭"
        ]


mbap : Parser (Maybe Accidental)
mbap =
    P.oneOf
        [ P.succeed Just |= accidentalParser
        , P.succeed Nothing
        ]


noteParser : Parser Note
noteParser =
    P.succeed (\pc ac -> { pitchClass = pc, accidental = ac })
        |= pitchParser
        |= mbap


slashNoteParser : Parser Note
slashNoteParser =
    P.succeed (\pc ac -> { pitchClass = pc, accidental = ac })
        |. P.symbol "/"
        |= pitchParser
        |= mbap


mbSn : Parser (Maybe Note)
mbSn =
    P.oneOf
        [ P.succeed Just |= slashNoteParser
        , P.succeed Nothing
        ]


chordParser : Parser ChordParse
chordParser =
    P.succeed (\rn qs sc -> { rootNote = rn, qualities = qs, slashNote = sc })
        |= noteParser
        |= listOf qualityParser
        |= mbSn


chordsParser : Parser (List ChordParse)
chordsParser =
    P.loop [] cpHelp


cpHelp : List ChordParse -> P.Parser (P.Step (List ChordParse) (List ChordParse))
cpHelp cps =
    P.oneOf
        [ P.succeed (\cp -> P.Loop (cp :: cps))
            |= chordParser
            |. P.spaces
        , P.succeed ()
            |> P.map (\_ -> P.Done (List.reverse cps))
        ]
