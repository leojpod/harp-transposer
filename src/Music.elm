module Music exposing
    ( Chromatic
    , HarmnonicaKey(..)
    , HarmonicaNote(..)
    , LickElement(..)
    , Position(..)
    , TransposedLick
    , TransposedLickElement(..)
    , TransposedNote
    , WrittenLick
    , allHarmonicaKeys
    , allPositions
    , circleOf5th
    , eighthPosition
    , eleventhPosition
    , fifthPosition
    , firstPosition
    , fourthPosition
    , ninthPosition
    , noteToString
    , parseLick
    , secondPosition
    , seventhPosition
    , sixthPosition
    , targetKeyAndPositon
    , tenthPosition
    , thirdPosition
    , toMusicPosition
    , transpose
    , twelfthPosition
    )

import List
import List.Extra as List
import Parser exposing ((|.), (|=), oneOf, succeed)


type Chromatic
    = -- NOTE: this needs to be between 0 and 12, should it be checked at compile time?
      C1
    | C2b
    | C2
    | C3b
    | C3
    | C4
    | C5b
    | C5
    | C6b
    | C6
    | C7b
    | C7


type HarmonicaNote
    = B1o
    | B1
    | D1
    | D1b
    | B2
    | D2
    | D2b
    | D2bb
    | B3
    | D3
    | D3b
    | D3bb
    | D3bbb
    | B4o
    | B4
    | D4
    | D4b
    | B5o
    | B5
    | D5
    | B6o
    | B6
    | D6
    | D6b
    | B7
    | D7
    | D7o
    | B8b
    | B8
    | D8
    | B9b
    | B9
    | D9
    | D9o
    | B10bb
    | B10b
    | B10
    | D10
    | D10o


noteToString : HarmonicaNote -> String
noteToString note =
    case note of
        B1o ->
            "+1°"

        B1 ->
            "+1"

        D1 ->
            "-1"

        D1b ->
            "-1'"

        B2 ->
            "+2"

        D2 ->
            "-2"

        D2b ->
            "-2'"

        D2bb ->
            "-2''"

        B3 ->
            "+3"

        D3 ->
            "-3"

        D3b ->
            "-3'"

        D3bb ->
            "-3''"

        D3bbb ->
            "-3'''"

        B4o ->
            "+4°"

        B4 ->
            "+4"

        D4 ->
            "-4"

        D4b ->
            "-4'"

        B5o ->
            "+5°"

        B5 ->
            "+5"

        D5 ->
            "-5"

        B6o ->
            "+6°"

        B6 ->
            "+6"

        D6 ->
            "-6"

        D6b ->
            "-6'"

        B7 ->
            "+7"

        D7 ->
            "-7"

        D7o ->
            "-7°"

        B8b ->
            "+8'"

        B8 ->
            "+8"

        D8 ->
            "-8"

        B9b ->
            "+9'"

        B9 ->
            "+9"

        D9 ->
            "-9"

        D9o ->
            "-9°"

        B10bb ->
            "+10''"

        B10b ->
            "+10'"

        B10 ->
            "+10"

        D10 ->
            "-10"

        D10o ->
            "-10°"


type alias MusicPosition =
    { toChromatic : HarmonicaNote -> Chromatic
    , fromChromatic : Chromatic -> List HarmonicaNote
    }


type alias WrittenLick =
    List LickElement


type LickElement
    = Annotation String
    | HarmonicaNote HarmonicaNote


parseLick : String -> Result String WrittenLick
parseLick lick =
    Parser.run lickParser lick
        |> Result.mapError
            (\deadend ->
                Parser.deadEndsToString <| deadend
            )


lickParser : Parser.Parser WrittenLick
lickParser =
    Parser.loop [] singleNoteParser


singleNoteParser : List LickElement -> Parser.Parser (Parser.Step (List LickElement) (List LickElement))
singleNoteParser revElements =
    Parser.oneOf
        [ Parser.end
            |> Parser.map
                (\_ ->
                    Parser.Done <| List.reverse revElements
                )
        , Parser.backtrackable (alternativeDrawNoteParser |> Parser.map (\note -> Parser.Loop (HarmonicaNote note :: revElements)))
        , Parser.backtrackable (alternativeBlowNoteParser |> Parser.map (\note -> Parser.Loop (HarmonicaNote note :: revElements)))
        , Parser.backtrackable (drawNoteParser |> Parser.map (\note -> Parser.Loop (HarmonicaNote note :: revElements)))
        , Parser.backtrackable (blowNoteParser |> Parser.map (\note -> Parser.Loop (HarmonicaNote note :: revElements)))
        , annotationParser |> Parser.map (\annotation -> Parser.Loop (Annotation annotation :: revElements))
        ]


alternativeDrawNoteParser : Parser.Parser HarmonicaNote
alternativeDrawNoteParser =
    Parser.succeed HoleAndAlterations
        |= parseHole
        |. Parser.symbol "D"
        |= parseAlteration
        |> Parser.andThen
            drawNoteFromHoleAndAlteration


drawNoteParser : Parser.Parser HarmonicaNote
drawNoteParser =
    Parser.succeed identity
        |. Parser.symbol "-"
        |= parserHoleAndAlteration
        |> Parser.andThen
            drawNoteFromHoleAndAlteration


drawNoteFromHoleAndAlteration : HoleAndAlterations -> Parser.Parser HarmonicaNote
drawNoteFromHoleAndAlteration { hole, alteration } =
    case ( hole, alteration ) of
        ( 1, None ) ->
            Parser.succeed D1

        ( 1, Bend 1 ) ->
            Parser.succeed D1b

        ( 2, None ) ->
            Parser.succeed D2

        ( 2, Bend 1 ) ->
            Parser.succeed D2b

        ( 2, Bend 2 ) ->
            Parser.succeed D2bb

        ( 3, None ) ->
            Parser.succeed D3

        ( 3, Bend 1 ) ->
            Parser.succeed D3b

        ( 3, Bend 2 ) ->
            Parser.succeed D3bb

        ( 3, Bend 3 ) ->
            Parser.succeed D3bbb

        ( 4, None ) ->
            Parser.succeed D4

        ( 4, Bend 1 ) ->
            Parser.succeed D4b

        ( 5, None ) ->
            Parser.succeed D5

        ( 6, None ) ->
            Parser.succeed D6

        ( 6, Bend 1 ) ->
            Parser.succeed D6b

        ( 7, None ) ->
            Parser.succeed D7

        ( 7, Over ) ->
            Parser.succeed D7o

        ( 8, None ) ->
            Parser.succeed D8

        ( 9, None ) ->
            Parser.succeed D9

        ( 9, Over ) ->
            Parser.succeed D9o

        ( 10, None ) ->
            Parser.succeed D10

        ( 10, Over ) ->
            Parser.succeed D10o

        _ ->
            Parser.problem <| "wrong combination of draw hold and alteration D" ++ String.fromInt hole ++ alterationToString alteration


alternativeBlowNoteParser : Parser.Parser HarmonicaNote
alternativeBlowNoteParser =
    Parser.succeed HoleAndAlterations
        |= parseHole
        |. Parser.symbol "B"
        |= parseAlteration
        |> Parser.andThen
            blowNoteFromHoleAndAlteration


blowNoteParser : Parser.Parser HarmonicaNote
blowNoteParser =
    succeed identity
        |. oneOf
            [ Parser.symbol "+"
            , Parser.succeed ()
            ]
        |= parserHoleAndAlteration
        |> Parser.andThen
            blowNoteFromHoleAndAlteration


blowNoteFromHoleAndAlteration : HoleAndAlterations -> Parser.Parser HarmonicaNote
blowNoteFromHoleAndAlteration { hole, alteration } =
    case ( hole, alteration ) of
        ( 1, Over ) ->
            Parser.succeed B1o

        ( 1, None ) ->
            Parser.succeed B1

        ( 2, None ) ->
            Parser.succeed B2

        ( 3, None ) ->
            Parser.succeed B3

        ( 4, Over ) ->
            Parser.succeed B4o

        ( 4, None ) ->
            Parser.succeed B4

        ( 5, Over ) ->
            Parser.succeed B5o

        ( 5, None ) ->
            Parser.succeed B5

        ( 6, Over ) ->
            Parser.succeed B6o

        ( 6, None ) ->
            Parser.succeed B6

        ( 7, None ) ->
            Parser.succeed B7

        ( 8, Bend 1 ) ->
            Parser.succeed B8b

        ( 8, None ) ->
            Parser.succeed B8

        ( 9, Bend 1 ) ->
            Parser.succeed B9b

        ( 9, None ) ->
            Parser.succeed B9

        ( 10, Bend 1 ) ->
            Parser.succeed B10b

        ( 10, Bend 2 ) ->
            Parser.succeed B10bb

        ( 10, None ) ->
            Parser.succeed B10

        _ ->
            Parser.problem <| "wrong combination of blow hole and alteration: B" ++ String.fromInt hole ++ alterationToString alteration


type Alteration
    = Bend Int
    | Over
    | None


alterationToString : Alteration -> String
alterationToString alteration =
    case alteration of
        Bend i ->
            String.repeat i "'"

        Over ->
            "°"

        None ->
            ""


type alias HoleAndAlterations =
    { hole : Int
    , alteration : Alteration
    }


parserHoleAndAlteration : Parser.Parser HoleAndAlterations
parserHoleAndAlteration =
    Parser.succeed HoleAndAlterations
        |= parseHole
        |= parseAlteration


parseHole : Parser.Parser Int
parseHole =
    Parser.getChompedString (Parser.chompWhile Char.isDigit)
        |> Parser.andThen
            (\str ->
                case String.toInt str of
                    Just int ->
                        Parser.succeed int

                    Nothing ->
                        Parser.problem "was expecting a number for the hole"
            )


parseAlteration : Parser.Parser Alteration
parseAlteration =
    Parser.oneOf
        [ parseBends |> Parser.map Bend
        , parseOver |> Parser.map (\_ -> Over)
        , Parser.succeed None
        ]


parseBends : Parser.Parser Int
parseBends =
    Parser.oneOf
        [ Parser.symbol "'''" |> Parser.map (\_ -> 3)
        , Parser.symbol "''" |> Parser.map (\_ -> 2)
        , Parser.symbol "'" |> Parser.map (\_ -> 1)
        , Parser.symbol "’’’" |> Parser.map (\_ -> 3)
        , Parser.symbol "’’" |> Parser.map (\_ -> 2)
        , Parser.symbol "’" |> Parser.map (\_ -> 1)
        , Parser.symbol "///" |> Parser.map (\_ -> 3)
        , Parser.symbol "//" |> Parser.map (\_ -> 2)
        , Parser.symbol "/" |> Parser.map (\_ -> 1)
        ]


parseOver : Parser.Parser ()
parseOver =
    Parser.symbol "°"


annotationParser : Parser.Parser String
annotationParser =
    Parser.getChompedString
        (Parser.succeed ()
            |. Parser.chompIf (\_ -> True)
            |. Parser.chompWhile (\c -> not (Char.isDigit c || c == '-' || c == '+'))
        )


firstPosition : MusicPosition
firstPosition =
    { toChromatic =
        \note ->
            case note of
                B1o ->
                    C3b

                B1 ->
                    C1

                D1 ->
                    C2

                D1b ->
                    C2b

                B2 ->
                    C3

                D2 ->
                    C5

                D2b ->
                    C5b

                D2bb ->
                    C4

                B3 ->
                    C3

                D3 ->
                    C7

                D3b ->
                    C7b

                D3bb ->
                    C6

                D3bbb ->
                    C6b

                B4o ->
                    C3b

                B4 ->
                    C1

                D4 ->
                    C2

                D4b ->
                    C2b

                B5o ->
                    C5b

                B5 ->
                    C3

                D5 ->
                    C4

                B6o ->
                    C7b

                B6 ->
                    C5

                D6 ->
                    C6

                D6b ->
                    C6b

                B7 ->
                    C1

                D7 ->
                    C7

                D7o ->
                    C2b

                B8b ->
                    C3b

                B8 ->
                    C3

                D8 ->
                    C2

                B9b ->
                    C5b

                B9 ->
                    C5

                D9 ->
                    C4

                D9o ->
                    C6b

                B10bb ->
                    C7b

                B10b ->
                    C7

                B10 ->
                    C1

                D10 ->
                    C6

                D10o ->
                    C2b
    , fromChromatic =
        \chromatic ->
            case chromatic of
                C1 ->
                    [ B1, B4, B7, B10 ]

                C2b ->
                    [ D1b, D4b, D7o, D10o ]

                C2 ->
                    [ D1, D4, D8 ]

                C3b ->
                    [ B1o, B4o, B8b ]

                C3 ->
                    [ B2, B5, B8 ]

                C4 ->
                    [ D2bb, D5, D9 ]

                C5b ->
                    [ D2b, B5o, B9b ]

                C5 ->
                    [ D2, B3, B6, B9 ]

                C6b ->
                    [ D3bbb, D6b ]

                C6 ->
                    [ D3bb, D6, D10 ]

                C7b ->
                    [ D3b, B6o, B10bb ]

                C7 ->
                    [ D3, D7, B10b ]
    }


secondPosition : MusicPosition
secondPosition =
    { toChromatic =
        \note ->
            case note of
                B1o ->
                    C6b

                B1 ->
                    C4

                D1 ->
                    C5

                D1b ->
                    C5b

                B2 ->
                    C6

                D2 ->
                    C1

                D2b ->
                    C7

                D2bb ->
                    C7b

                B3 ->
                    C1

                D3 ->
                    C3

                D3b ->
                    C3b

                D3bb ->
                    C2

                D3bbb ->
                    C2b

                B4o ->
                    C6b

                B4 ->
                    C4

                D4 ->
                    C5

                D4b ->
                    C5b

                B5o ->
                    C7

                B5 ->
                    C6

                D5 ->
                    C7b

                B6o ->
                    C3b

                B6 ->
                    C1

                D6 ->
                    C2

                D6b ->
                    C2b

                B7 ->
                    C4

                D7 ->
                    C3

                D7o ->
                    C5b

                B8b ->
                    C6b

                B8 ->
                    C6

                D8 ->
                    C5

                B9b ->
                    C7

                B9 ->
                    C1

                D9 ->
                    C7b

                D9o ->
                    C2b

                B10bb ->
                    C3b

                B10b ->
                    C3

                B10 ->
                    C4

                D10 ->
                    C2

                D10o ->
                    C5b
    , fromChromatic =
        \chromatic ->
            case chromatic of
                C1 ->
                    [ D2, B3, B6, B9 ]

                C2b ->
                    [ D3bbb, D6b, D9o ]

                C2 ->
                    [ D3bb, D6, D10 ]

                C3b ->
                    [ D3b, B6o, B10bb ]

                C3 ->
                    [ D3, D7, B10b ]

                C4 ->
                    [ B1, B4, B7, B10 ]

                C5b ->
                    [ D1b, D4b, D7o, D10o ]

                C5 ->
                    [ D1, D4, D8 ]

                C6b ->
                    [ B1o, B4o, B8b ]

                C6 ->
                    [ B2, B5, B8 ]

                C7b ->
                    [ D2bb, D5, D9 ]

                C7 ->
                    [ D2b, B5o, B9b ]
    }


thirdPosition : MusicPosition
thirdPosition =
    { toChromatic =
        \note ->
            case note of
                B1o ->
                    C2b

                B1 ->
                    C7

                D1 ->
                    C1

                D1b ->
                    C5

                B2 ->
                    C2

                D2 ->
                    C4

                D2b ->
                    C3

                D2bb ->
                    C3b

                B3 ->
                    C4

                D3 ->
                    C6

                D3b ->
                    C6b

                D3bb ->
                    C5

                D3bbb ->
                    C5b

                B4o ->
                    C2b

                B4 ->
                    C7b

                D4 ->
                    C1

                D4b ->
                    C7

                B5o ->
                    C3

                B5 ->
                    C2

                D5 ->
                    C3b

                B6o ->
                    C6b

                B6 ->
                    C4

                D6 ->
                    C5

                D6b ->
                    C5b

                B7 ->
                    C7b

                D7 ->
                    C6

                D7o ->
                    C7

                B8b ->
                    C2b

                B8 ->
                    C2

                D8 ->
                    C1

                B9b ->
                    C3

                B9 ->
                    C4

                D9 ->
                    C3b

                D9o ->
                    C5b

                B10bb ->
                    C6b

                B10b ->
                    C6

                B10 ->
                    C7b

                D10 ->
                    C5

                D10o ->
                    C7
    , fromChromatic =
        \chromatic ->
            case chromatic of
                C1 ->
                    [ D1, D4, D8 ]

                C2b ->
                    [ B1o, B4o, B8b ]

                C2 ->
                    [ B2, B5, B8 ]

                C3b ->
                    [ D2bb, D5, D9 ]

                C3 ->
                    [ D2b, B5o, B9b ]

                C4 ->
                    [ D2, B6, B9 ]

                C5b ->
                    [ D3bbb, D6b, D9o ]

                C5 ->
                    [ D3b, D6, D10 ]

                C6b ->
                    [ D3b, B6o, B10bb ]

                C6 ->
                    [ D3, D7, B10b ]

                C7b ->
                    [ B1, B4, B7, B10 ]

                C7 ->
                    [ D1b, D4b, D7o, D10o ]
    }


fourthPosition : MusicPosition
fourthPosition =
    { toChromatic =
        \note ->
            case note of
                B1o ->
                    C5b

                B1 ->
                    C3b

                D1 ->
                    C4

                D1b ->
                    C3

                B2 ->
                    C5

                D2 ->
                    C7b

                D2b ->
                    C6

                D2bb ->
                    C6b

                B3 ->
                    C7b

                D3 ->
                    C2

                D3b ->
                    C2b

                D3bb ->
                    C1

                D3bbb ->
                    C7

                B4o ->
                    C5b

                B4 ->
                    C3b

                D4 ->
                    C4

                D4b ->
                    C3

                B5o ->
                    C6

                B5 ->
                    C5

                D5 ->
                    C6b

                B6o ->
                    C2b

                B6 ->
                    C7b

                D6 ->
                    C1

                D6b ->
                    C7

                B7 ->
                    C3b

                D7 ->
                    C2

                D7o ->
                    C3

                B8b ->
                    C5b

                B8 ->
                    C5

                D8 ->
                    C4

                B9b ->
                    C6

                B9 ->
                    C7b

                D9 ->
                    C6b

                D9o ->
                    C7

                B10bb ->
                    C2b

                B10b ->
                    C2

                B10 ->
                    C3b

                D10 ->
                    C1

                D10o ->
                    C3
    , fromChromatic =
        \chromatic ->
            case chromatic of
                C1 ->
                    [ D3bb, D6, D10 ]

                C2b ->
                    [ D3b, B6o, B10bb ]

                C2 ->
                    [ D3, D7, B10b ]

                C3b ->
                    [ B1, B4, B7, B10 ]

                C3 ->
                    [ D1b, D4b, D7o, D10o ]

                C4 ->
                    [ D1, D4, D8 ]

                C5b ->
                    [ B1o, B4o, B8b ]

                C5 ->
                    [ B2, B5, B8 ]

                C6b ->
                    [ D2bb, D5, D9 ]

                C6 ->
                    [ D2b, B5o, B9b ]

                C7b ->
                    [ D2, B3, B6, B9 ]

                C7 ->
                    [ D3bbb, D6b, D9o ]
    }


fifthPosition : MusicPosition
fifthPosition =
    { toChromatic =
        \note ->
            case note of
                B1o ->
                    C7

                B1 ->
                    C6b

                D1 ->
                    C7b

                D1b ->
                    C6

                B2 ->
                    C1

                D2 ->
                    C3b

                D2b ->
                    C2

                D2bb ->
                    C2b

                B3 ->
                    C3b

                D3 ->
                    C5

                D3b ->
                    C5b

                D3bb ->
                    C4

                D3bbb ->
                    C3

                B4o ->
                    C7

                B4 ->
                    C6b

                D4 ->
                    C7b

                D4b ->
                    C6

                B5o ->
                    C2

                B5 ->
                    C1

                D5 ->
                    C2b

                B6o ->
                    C5b

                B6 ->
                    C3b

                D6 ->
                    C4

                D6b ->
                    C3

                B7 ->
                    C6b

                D7 ->
                    C5

                D7o ->
                    C6

                B8b ->
                    C7

                B8 ->
                    C1

                D8 ->
                    C7b

                B9b ->
                    C2

                B9 ->
                    C3b

                D9 ->
                    C2b

                D9o ->
                    C3

                B10bb ->
                    C5b

                B10b ->
                    C5

                B10 ->
                    C6b

                D10 ->
                    C4

                D10o ->
                    C6
    , fromChromatic =
        \chromatic ->
            case chromatic of
                C1 ->
                    [ B2, B5, B8 ]

                C2b ->
                    [ D2bb, D5, D9 ]

                C2 ->
                    [ D2b, B5o, B9b ]

                C3b ->
                    [ D2, B3, B6, B9 ]

                C3 ->
                    [ D3bbb, D6b, D9o ]

                C4 ->
                    [ D3bb, D6, D10 ]

                C5b ->
                    [ D3b, B6o, B10bb ]

                C5 ->
                    [ D3, D7, B10b ]

                C6b ->
                    [ B1, B4, B7, B10 ]

                C6 ->
                    [ D1b, D4b, D7o, D10o ]

                C7b ->
                    [ D1, D4, D8 ]

                C7 ->
                    [ B1o, B4o, B8b ]
    }


sixthPosition : MusicPosition
sixthPosition =
    { toChromatic =
        \note ->
            case note of
                B1o ->
                    C3

                B1 ->
                    C2b

                D1 ->
                    C3b

                D1b ->
                    C2

                B2 ->
                    C4

                D2 ->
                    C6b

                D2b ->
                    C5

                D2bb ->
                    C5b

                B3 ->
                    C6b

                D3 ->
                    C1

                D3b ->
                    C7

                D3bb ->
                    C7b

                D3bbb ->
                    C6

                B4o ->
                    C3

                B4 ->
                    C2b

                D4 ->
                    C3b

                D4b ->
                    C2

                B5o ->
                    C5

                B5 ->
                    C4

                D5 ->
                    C5b

                B6o ->
                    C7

                B6 ->
                    C6b

                D6 ->
                    C7b

                D6b ->
                    C6

                B7 ->
                    C2b

                D7 ->
                    C1

                D7o ->
                    C2

                B8b ->
                    C3

                B8 ->
                    C4

                D8 ->
                    C3b

                B9b ->
                    C5

                B9 ->
                    C6b

                D9 ->
                    C5b

                D9o ->
                    C6

                B10bb ->
                    C7

                B10b ->
                    C1

                B10 ->
                    C2b

                D10 ->
                    C7b

                D10o ->
                    C2
    , fromChromatic =
        \chromatic ->
            case chromatic of
                C1 ->
                    [ D3, D7, B10b ]

                C2b ->
                    [ B1, B4, B7, B10 ]

                C2 ->
                    [ D1b, D4b, D7o, D10o ]

                C3b ->
                    [ D1, D4, D8 ]

                C3 ->
                    [ B1o, B4o, B8b ]

                C4 ->
                    [ B2, B5, B8 ]

                C5b ->
                    [ D2bb, D5, D9 ]

                C5 ->
                    [ D2b, B5o, B9b ]

                C6b ->
                    [ D2, B3, B6, B9 ]

                C6 ->
                    [ D3bbb, D6b, D9o ]

                C7b ->
                    [ D3bb, D6, D10 ]

                C7 ->
                    [ D3b, B6o, B10bb ]
    }


seventhPosition : MusicPosition
seventhPosition =
    { toChromatic =
        \note ->
            case note of
                B1o ->
                    C6

                B1 ->
                    C5b

                D1 ->
                    C6b

                D1b ->
                    C5

                B2 ->
                    C7b

                D2 ->
                    C2b

                D2b ->
                    C1

                D2bb ->
                    C7

                B3 ->
                    C2b

                D3 ->
                    C4

                D3b ->
                    C3

                D3bb ->
                    C3b

                D3bbb ->
                    C2

                B4o ->
                    C6

                B4 ->
                    C5b

                D4 ->
                    C6b

                D4b ->
                    C5

                B5o ->
                    C1

                B5 ->
                    C7b

                D5 ->
                    C7

                B6o ->
                    C3

                B6 ->
                    C2b

                D6 ->
                    C3b

                D6b ->
                    C2

                B7 ->
                    C5b

                D7 ->
                    C4

                D7o ->
                    C5

                B8b ->
                    C6

                B8 ->
                    C7b

                D8 ->
                    C6b

                B9b ->
                    C1

                B9 ->
                    C2b

                D9 ->
                    C7

                D9o ->
                    C2

                B10bb ->
                    C3

                B10b ->
                    C4

                B10 ->
                    C5b

                D10 ->
                    C3b

                D10o ->
                    C5
    , fromChromatic =
        \chromatic ->
            case chromatic of
                C1 ->
                    [ D2b, B5o, B9b ]

                C2b ->
                    [ D2, B3, B6, B9 ]

                C2 ->
                    [ D3bbb, D6b, D9o ]

                C3b ->
                    [ D3bb, D6, D10 ]

                C3 ->
                    [ D3b, B6o, B10bb ]

                C4 ->
                    [ D3, D7, B10b ]

                C5b ->
                    [ B1, B4, B7, B10 ]

                C5 ->
                    [ D1b, D4b, D7o, D10o ]

                C6b ->
                    [ D1, D4, D8 ]

                C6 ->
                    [ B1o, B4o, B8b ]

                C7b ->
                    [ B2, B5, B8 ]

                C7 ->
                    [ D2bb, D5, D9 ]
    }


eighthPosition : MusicPosition
eighthPosition =
    { toChromatic =
        \note ->
            case note of
                B1o ->
                    C2

                B1 ->
                    C7

                D1 ->
                    C2b

                D1b ->
                    C1

                B2 ->
                    C3b

                D2 ->
                    C5b

                D2b ->
                    C4

                D2bb ->
                    C3

                B3 ->
                    C5b

                D3 ->
                    C7b

                D3b ->
                    C6

                D3bb ->
                    C6b

                D3bbb ->
                    C5

                B4o ->
                    C2

                B4 ->
                    C7

                D4 ->
                    C2b

                D4b ->
                    C1

                B5o ->
                    C4

                B5 ->
                    C3b

                D5 ->
                    C3

                B6o ->
                    C6

                B6 ->
                    C5b

                D6 ->
                    C6b

                D6b ->
                    C5

                B7 ->
                    C7

                D7 ->
                    C7b

                D7o ->
                    C1

                B8b ->
                    C2

                B8 ->
                    C3b

                D8 ->
                    C2b

                B9b ->
                    C4

                B9 ->
                    C5b

                D9 ->
                    C3

                D9o ->
                    C5

                B10bb ->
                    C6

                B10b ->
                    C7b

                B10 ->
                    C7

                D10 ->
                    C6b

                D10o ->
                    C1
    , fromChromatic =
        \chromatic ->
            case chromatic of
                C1 ->
                    [ D1b, D4b, D7o, D10o ]

                C2b ->
                    [ D1, D4, D8 ]

                C2 ->
                    [ B1o, B4o, B8b ]

                C3b ->
                    [ B2, B5, B8 ]

                C3 ->
                    [ D2bb, D5, D9 ]

                C4 ->
                    [ D2b, B5o, B9b ]

                C5b ->
                    [ D2, B3, B6, B9 ]

                C5 ->
                    [ D3bbb, D6b, D9o ]

                C6b ->
                    [ D3bb, D6, D10 ]

                C6 ->
                    [ D3b, B6o, B10bb ]

                C7b ->
                    [ D3, D7, B10b ]

                C7 ->
                    [ B1, B4, B7, B10 ]
    }


ninthPosition : MusicPosition
ninthPosition =
    { toChromatic =
        \note ->
            case note of
                B1o ->
                    C5

                B1 ->
                    C3

                D1 ->
                    C5b

                D1b ->
                    C4

                B2 ->
                    C6b

                D2 ->
                    C7

                D2b ->
                    C7b

                D2bb ->
                    C6

                B3 ->
                    C7

                D3 ->
                    C3b

                D3b ->
                    C2

                D3bb ->
                    C2b

                D3bbb ->
                    C1

                B4o ->
                    C5

                B4 ->
                    C3

                D4 ->
                    C5b

                D4b ->
                    C4

                B5o ->
                    C7b

                B5 ->
                    C6b

                D5 ->
                    C6

                B6o ->
                    C2

                B6 ->
                    C7

                D6 ->
                    C2b

                D6b ->
                    C1

                B7 ->
                    C3

                D7 ->
                    C3b

                D7o ->
                    C4

                B8b ->
                    C5

                B8 ->
                    C6b

                D8 ->
                    C5b

                B9b ->
                    C7b

                B9 ->
                    C7

                D9 ->
                    C6

                D9o ->
                    C1

                B10bb ->
                    C2

                B10b ->
                    C3b

                B10 ->
                    C3

                D10 ->
                    C2b

                D10o ->
                    C4
    , fromChromatic =
        \chromatic ->
            case chromatic of
                C1 ->
                    [ D3bbb, D6b, D9o ]

                C2b ->
                    [ D3bb, D6, D10 ]

                C2 ->
                    [ D3b, B6o, B10bb ]

                C3b ->
                    [ D3, D7, B10b ]

                C3 ->
                    [ B1, B4, B7, B10 ]

                C4 ->
                    [ D1b, D4b, D7o, D10o ]

                C5b ->
                    [ D1, D4, D8 ]

                C5 ->
                    [ B1o, B4o, B8b ]

                C6b ->
                    [ B2, B5, B8 ]

                C6 ->
                    [ D2bb, D5, D9 ]

                C7b ->
                    [ D2b, B5o, B9b ]

                C7 ->
                    [ D2, B3, B6, B9 ]
    }


tenthPosition : MusicPosition
tenthPosition =
    { toChromatic =
        \note ->
            case note of
                B1o ->
                    C1

                B1 ->
                    C6

                D1 ->
                    C7

                D1b ->
                    C7b

                B2 ->
                    C2b

                D2 ->
                    C3

                D2b ->
                    C3b

                D2bb ->
                    C2

                B3 ->
                    C3

                D3 ->
                    C6b

                D3b ->
                    C5

                D3bb ->
                    C5b

                D3bbb ->
                    C4

                B4o ->
                    C1

                B4 ->
                    C6

                D4 ->
                    C7

                D4b ->
                    C7b

                B5o ->
                    C3b

                B5 ->
                    C2b

                D5 ->
                    C2

                B6o ->
                    C5

                B6 ->
                    C3

                D6 ->
                    C5b

                D6b ->
                    C4

                B7 ->
                    C6

                D7 ->
                    C6b

                D7o ->
                    C7b

                B8b ->
                    C1

                B8 ->
                    C2b

                D8 ->
                    C7

                B9b ->
                    C3b

                B9 ->
                    C3

                D9 ->
                    C2

                D9o ->
                    C4

                B10bb ->
                    C5

                B10b ->
                    C6b

                B10 ->
                    C6

                D10 ->
                    C5b

                D10o ->
                    C7b
    , fromChromatic =
        \chromatic ->
            case chromatic of
                C1 ->
                    [ B1o, B4o, B8b ]

                C2b ->
                    [ B2, B5, B8 ]

                C2 ->
                    [ D2bb, D5, D9 ]

                C3b ->
                    [ D2b, B5o, B9b ]

                C3 ->
                    [ D2, B3, B6, B9 ]

                C4 ->
                    [ D3bbb, D6b, D9o ]

                C5b ->
                    [ D3bb, D6, D10 ]

                C5 ->
                    [ D3b, B6o, B10bb ]

                C6b ->
                    [ D3, D7, B10b ]

                C6 ->
                    [ B1, B4, B7, B10 ]

                C7b ->
                    [ D1b, D4b, D7o, D10o ]

                C7 ->
                    [ D1, D4, D8 ]
    }


eleventhPosition : MusicPosition
eleventhPosition =
    { toChromatic =
        \note ->
            case note of
                B1o ->
                    C4

                B1 ->
                    C2

                D1 ->
                    C3

                D1b ->
                    C3b

                B2 ->
                    C5b

                D2 ->
                    C6

                D2b ->
                    C6b

                D2bb ->
                    C5

                B3 ->
                    C6

                D3 ->
                    C2b

                D3b ->
                    C1

                D3bb ->
                    C7

                D3bbb ->
                    C7b

                B4o ->
                    C4

                B4 ->
                    C2

                D4 ->
                    C3

                D4b ->
                    C3b

                B5o ->
                    C6b

                B5 ->
                    C5b

                D5 ->
                    C5

                B6o ->
                    C1

                B6 ->
                    C6

                D6 ->
                    C7

                D6b ->
                    C7b

                B7 ->
                    C2

                D7 ->
                    C2b

                D7o ->
                    C3b

                B8b ->
                    C4

                B8 ->
                    C5b

                D8 ->
                    C3

                B9b ->
                    C6b

                B9 ->
                    C6

                D9 ->
                    C5

                D9o ->
                    C7b

                B10bb ->
                    C1

                B10b ->
                    C2b

                B10 ->
                    C2

                D10 ->
                    C7

                D10o ->
                    C3b
    , fromChromatic =
        \chromatic ->
            case chromatic of
                C1 ->
                    [ D3b, B6o, B10bb ]

                C2b ->
                    [ D3, D7, B10b ]

                C2 ->
                    [ B1, B4, B7, B10 ]

                C3b ->
                    [ D1b, D4b, D7o, D10o ]

                C3 ->
                    [ D1, D4, D8 ]

                C4 ->
                    [ B1o, B4o, B8b ]

                C5b ->
                    [ B2, B5, B8 ]

                C5 ->
                    [ D2bb, D5, D9 ]

                C6b ->
                    [ D2b, B5o, B9b ]

                C6 ->
                    [ D2, B3, B6, B9 ]

                C7b ->
                    [ D3bbb, D6b, D9o ]

                C7 ->
                    [ D3bb, D6, D10 ]
    }


twelfthPosition : MusicPosition
twelfthPosition =
    { toChromatic =
        \note ->
            case note of
                B1o ->
                    C7b

                B1 ->
                    C5

                D1 ->
                    C6

                D1b ->
                    C6b

                B2 ->
                    C7

                D2 ->
                    C2

                D2b ->
                    C2b

                D2bb ->
                    C1

                B3 ->
                    C2

                D3 ->
                    C5b

                D3b ->
                    C4

                D3bb ->
                    C3

                D3bbb ->
                    C3b

                B4o ->
                    C7b

                B4 ->
                    C5

                D4 ->
                    C6

                D4b ->
                    C6b

                B5o ->
                    C2b

                B5 ->
                    C7

                D5 ->
                    C1

                B6o ->
                    C4

                B6 ->
                    C2

                D6 ->
                    C3

                D6b ->
                    C3b

                B7 ->
                    C5

                D7 ->
                    C5b

                D7o ->
                    C6b

                B8b ->
                    C7b

                B8 ->
                    C7

                D8 ->
                    C6

                B9b ->
                    C2b

                B9 ->
                    C2

                D9 ->
                    C1

                D9o ->
                    C3b

                B10bb ->
                    C4

                B10b ->
                    C5b

                B10 ->
                    C5

                D10 ->
                    C3

                D10o ->
                    C6b
    , fromChromatic =
        \chromatic ->
            case chromatic of
                C1 ->
                    [ D2bb, D5, D9 ]

                C2b ->
                    [ D2b, B5o, B9b ]

                C2 ->
                    [ D2, B3, B6, B9 ]

                C3b ->
                    [ D3bbb, D6b, D9o ]

                C3 ->
                    [ D3bb, D6, D10 ]

                C4 ->
                    [ D3b, B6o, B10bb ]

                C5b ->
                    [ D3, D7, B10b ]

                C5 ->
                    [ B1, B4, B7, B10 ]

                C6b ->
                    [ D1b, D4b, D7o, D10o ]

                C6 ->
                    [ D1, D4, D8 ]

                C7b ->
                    [ B1o, B4o, B8b ]

                C7 ->
                    [ B2, B5, B8 ]
    }


type alias TransposedLick =
    List TransposedLickElement


type alias TransposedNote =
    { original : HarmonicaNote
    , options : List HarmonicaNote
    }


type TransposedLickElement
    = NoteOptions TransposedNote
    | Annotation_ String


transpose : MusicPosition -> MusicPosition -> WrittenLick -> TransposedLick
transpose from to =
    List.map
        (\element ->
            case element of
                HarmonicaNote note ->
                    NoteOptions
                        { original = note
                        , options = transposeNote from to note
                        }

                Annotation annotation ->
                    Annotation_ annotation
        )


transposeNote : MusicPosition -> MusicPosition -> HarmonicaNote -> List HarmonicaNote
transposeNote { toChromatic } { fromChromatic } note =
    toChromatic note |> fromChromatic


type HarmnonicaKey
    = G
    | Ab
    | A
    | Bb
    | B
    | C
    | Db
    | D
    | Eb
    | E
    | F
    | Gb


keyToString : HarmnonicaKey -> String
keyToString key =
    case key of
        G ->
            "G"

        Ab ->
            "Ab/G#"

        A ->
            "A"

        Bb ->
            "Bb/A#"

        B ->
            "B"

        C ->
            "C"

        Db ->
            "Db/C#"

        D ->
            "D"

        Eb ->
            "Eb/D#"

        E ->
            "E"

        F ->
            "F"

        Gb ->
            "Gb/F#"


allHarmonicaKeys : List ( String, HarmnonicaKey )
allHarmonicaKeys =
    [ G
    , Ab
    , A
    , Bb
    , B
    , C
    , Db
    , D
    , Eb
    , E
    , F
    , Gb
    ]
        |> List.map
            (\key -> ( keyToString key, key ))


circleOf5th : List HarmnonicaKey
circleOf5th =
    [ F, Bb, Eb, Ab, Db, Gb, B, E, A, D, G, C ]


type Position
    = FirstPos
    | SecondPos
    | ThirdPos
    | FourthPos
    | FifthPos
    | SixthPos
    | SeventhPos
    | EighthPos
    | NinthPos
    | TenthPos
    | EleventhPos
    | TwelfthPos


allPositions : List ( String, Position )
allPositions =
    [ ( "1st", FirstPos )
    , ( "2nd", SecondPos )
    , ( "3rd", ThirdPos )
    , ( "4th", FourthPos )
    , ( "5th", FifthPos )
    , ( "6th", SixthPos )
    , ( "7th", SeventhPos )
    , ( "8th", EighthPos )
    , ( "9th", NinthPos )
    , ( "10th", TenthPos )
    , ( "11th", EleventhPos )
    , ( "12th", TwelfthPos )
    ]


toMusicPosition : Position -> MusicPosition
toMusicPosition position =
    case position of
        FirstPos ->
            firstPosition

        SecondPos ->
            secondPosition

        ThirdPos ->
            thirdPosition

        FourthPos ->
            fourthPosition

        FifthPos ->
            fifthPosition

        SixthPos ->
            sixthPosition

        SeventhPos ->
            seventhPosition

        EighthPos ->
            eighthPosition

        NinthPos ->
            ninthPosition

        TenthPos ->
            tenthPosition

        EleventhPos ->
            eleventhPosition

        TwelfthPos ->
            twelfthPosition


targetKeyAndPositon : { key : HarmnonicaKey, position : Position } -> List ( String, Position )
targetKeyAndPositon from =
    let
        positionOffset : Int
        positionOffset =
            case from.position of
                FirstPos ->
                    0

                SecondPos ->
                    1

                ThirdPos ->
                    2

                FourthPos ->
                    3

                FifthPos ->
                    4

                SixthPos ->
                    5

                SeventhPos ->
                    6

                EighthPos ->
                    7

                NinthPos ->
                    8

                TenthPos ->
                    9

                EleventhPos ->
                    10

                TwelfthPos ->
                    11

        lickKey : HarmnonicaKey
        lickKey =
            (circleOf5th ++ circleOf5th)
                |> List.reverse
                |> List.dropWhile ((/=) from.key)
                |> List.getAt positionOffset
                |> Maybe.withDefault C

        adjustedCircleOf5th : List HarmnonicaKey
        adjustedCircleOf5th =
            circleOf5th
                ++ circleOf5th
                |> List.dropWhile ((/=) lickKey)
                |> List.splitAt 12
                |> Tuple.first

        indexedPositions : List ( Int, ( String, Position ) )
        indexedPositions =
            allPositions
                |> List.indexedMap (\idx posAndName -> ( idx, posAndName ))
    in
    List.zip adjustedCircleOf5th indexedPositions
        |> List.sortBy (\( _, ( index, _ ) ) -> index)
        |> List.map
            (\( key, ( _, ( posName, pos ) ) ) ->
                ( keyToString key ++ " (" ++ posName ++ " pos)", pos )
            )
