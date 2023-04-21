module Music exposing (..)

import Browser exposing (element)
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


allNotes : List HarmonicaNote
allNotes =
    [ B1o, B1, D1, D1b, B2, D2, D2b, D2bb, B3, D3, D3b, D3bb, D3bbb, B4o, B4, D4, D4b, B5o, B5, D5, B6o, B6, D6, D6b, B7, D7, D7o, B8b, B8, D8, B9b, B9, D9, D9o, B10bb, B10b, B10, D10, D10o ]


type alias Position =
    { to : HarmonicaNote -> Chromatic
    , from : Chromatic -> List HarmonicaNote
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
                Parser.deadEndsToString <| Debug.log "deadend? " deadend
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
        , drawNoteParser |> Parser.map (\note -> Parser.Loop (HarmonicaNote note :: revElements))
        , blowNoteParser |> Parser.map (\note -> Parser.Loop (HarmonicaNote note :: revElements))
        , annotationParser |> Parser.map (\annotation -> Parser.Loop (Annotation annotation :: revElements))
        ]


drawNoteParser : Parser.Parser HarmonicaNote
drawNoteParser =
    succeed identity
        |. Parser.symbol "-"
        |= parserHoleAndAlteration
        |> Parser.andThen
            (\{ hole, alteration } ->
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
            )


blowNoteParser : Parser.Parser HarmonicaNote
blowNoteParser =
    succeed identity
        |. oneOf
            [ Parser.symbol "+"
            , Parser.succeed ()
            ]
        |= parserHoleAndAlteration
        |> Parser.andThen
            (\{ hole, alteration } ->
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
            )


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
        |= Parser.oneOf
            [ parseBends |> Parser.map Bend
            , parseOver |> Parser.map (\_ -> Over)
            , Parser.succeed None
            ]


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


parseBends : Parser.Parser Int
parseBends =
    Parser.oneOf
        [ Parser.symbol "'''" |> Parser.map (\_ -> 3)
        , Parser.symbol "''" |> Parser.map (\_ -> 2)
        , Parser.symbol "'" |> Parser.map (\_ -> 1)
        ]


parseOver : Parser.Parser ()
parseOver =
    Parser.symbol "°"


annotationParser : Parser.Parser String
annotationParser =
    Parser.getChompedString <|
        Parser.chompWhile (\c -> not (Char.isDigit c || c == '-' || c == '+'))


writtenLickToString : WrittenLick -> String
writtenLickToString =
    List.map
        (\element ->
            case element of
                Annotation str ->
                    str

                HarmonicaNote note ->
                    noteToString note
        )
        >> String.join ""


firstPosition : Position
firstPosition =
    { to =
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
    , from =
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


secondPosition : Position
secondPosition =
    { to =
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
    , from =
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
