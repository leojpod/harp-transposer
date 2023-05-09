module MusicTest exposing (all)

import Expect
import Music exposing (parseLick)
import Test exposing (Test, describe, test)


all : Test
all =
    describe "All tests regarding the Music module"
        [ lickParser ]


lickParser : Test
lickParser =
    describe "Test lib parsing"
        [ everySingleNote
        , simpleLickParsing
        , alternativesNotationParsing
        ]


simpleLickParsing : Test
simpleLickParsing =
    describe "Simple examples of parsing"
        [ simpleTest "Empty lick" "" <| Ok []
        , simpleTest "simple single note" "4" <| Ok [ Music.HarmonicaNote Music.B4 ]
        , simpleTest "single draw note" "-4" <| Ok [ Music.HarmonicaNote Music.D4 ]
        , simpleTest "with a bend" "-3'" <| Ok [ Music.HarmonicaNote Music.D3b ]
        , simpleTest "only gibberish" "whatever" <| Ok [ Music.Annotation "whatever" ]
        , simpleTest "2 blows" "3 4" <| Ok [ Music.HarmonicaNote Music.B3, Music.Annotation " ", Music.HarmonicaNote Music.B4 ]
        , simpleTest "2 draws" "-3 -4" <| Ok [ Music.HarmonicaNote Music.D3, Music.Annotation " ", Music.HarmonicaNote Music.D4 ]
        , simpleTest "a blues lick" "-2 -3' +4 -4' -4 6 oh yeah" <|
            Ok
                [ Music.HarmonicaNote Music.D2
                , Music.Annotation " "
                , Music.HarmonicaNote Music.D3b
                , Music.Annotation " "
                , Music.HarmonicaNote Music.B4
                , Music.Annotation " "
                , Music.HarmonicaNote Music.D4b
                , Music.Annotation " "
                , Music.HarmonicaNote Music.D4
                , Music.Annotation " "
                , Music.HarmonicaNote Music.B6
                , Music.Annotation " oh yeah"
                ]
        ]


alternativesNotationParsing : Test
alternativesNotationParsing =
    describe "Simple examples of alternative notation parsing"
        [ simpleTest "simple single note" "4B" <| Ok [ Music.HarmonicaNote Music.B4 ]
        , simpleTest "single draw note" "4D" <| Ok [ Music.HarmonicaNote Music.D4 ]
        , simpleTest "with a bend" "3D'" <| Ok [ Music.HarmonicaNote Music.D3b ]
        , simpleTest "with a bigger bend" "3D’’’" <| Ok [ Music.HarmonicaNote Music.D3bbb ]
        , simpleTest "only gibberish" "whatever" <| Ok [ Music.Annotation "whatever" ]
        , simpleTest "2 blows" "3B 4B" <| Ok [ Music.HarmonicaNote Music.B3, Music.Annotation " ", Music.HarmonicaNote Music.B4 ]
        , simpleTest "2 draws" "3D 4D" <| Ok [ Music.HarmonicaNote Music.D3, Music.Annotation " ", Music.HarmonicaNote Music.D4 ]
        , simpleTest "a blues lick" "2D 3D’’ 4B 4D' 4D 6B oh yeah" <|
            Ok
                [ Music.HarmonicaNote Music.D2
                , Music.Annotation " "
                , Music.HarmonicaNote Music.D3bb
                , Music.Annotation " "
                , Music.HarmonicaNote Music.B4
                , Music.Annotation " "
                , Music.HarmonicaNote Music.D4b
                , Music.Annotation " "
                , Music.HarmonicaNote Music.D4
                , Music.Annotation " "
                , Music.HarmonicaNote Music.B6
                , Music.Annotation " oh yeah"
                ]
        , simpleTest "mixing both notation" "-3 3D' +4 5B 4D'" <|
            Ok
                [ Music.HarmonicaNote Music.D3
                , Music.Annotation " "
                , Music.HarmonicaNote Music.D3b
                , Music.Annotation " "
                , Music.HarmonicaNote Music.B4
                , Music.Annotation " "
                , Music.HarmonicaNote Music.B5
                , Music.Annotation " "
                , Music.HarmonicaNote Music.D4b
                ]
        ]


everySingleNote : Test
everySingleNote =
    let
        noteTest : String -> Music.HarmonicaNote -> Test
        noteTest noteStr note =
            test noteStr <|
                \_ ->
                    Music.parseLick noteStr
                        |> Expect.equal (Ok [ Music.HarmonicaNote note ])
    in
    describe "parsing every single note"
        [ noteTest "+1" Music.B1
        , noteTest "+1°" Music.B1o
        , noteTest "-1" Music.D1
        , noteTest "-1'" Music.D1b
        , noteTest "2" Music.B2
        , noteTest "-2" Music.D2
        , noteTest "-2'" Music.D2b
        , noteTest "-2''" Music.D2bb
        , noteTest "3" Music.B3
        , noteTest "-3" Music.D3
        , noteTest "-3'" Music.D3b
        , noteTest "-3''" Music.D3bb
        , noteTest "-3'''" Music.D3bbb
        , noteTest "+4" Music.B4
        , noteTest "+4°" Music.B4o
        , noteTest "-4" Music.D4
        , noteTest "-4'" Music.D4b
        , noteTest "+5" Music.B5
        , noteTest "+5°" Music.B5o
        , noteTest "-5" Music.D5
        , noteTest "+6" Music.B6
        , noteTest "+6°" Music.B6o
        , noteTest "-6" Music.D6
        , noteTest "-6'" Music.D6b
        , noteTest "+7" Music.B7
        , noteTest "-7" Music.D7
        , noteTest "-7°" Music.D7o
        , noteTest "+8'" Music.B8b
        , noteTest "+8" Music.B8
        , noteTest "-8" Music.D8
        , noteTest "+9'" Music.B9b
        , noteTest "+9" Music.B9
        , noteTest "-9" Music.D9
        , noteTest "-9°" Music.D9o
        , noteTest "+10''" Music.B10bb
        , noteTest "+10'" Music.B10b
        , noteTest "+10" Music.B10
        , noteTest "-10" Music.D10
        , noteTest "-10°" Music.D10o
        ]


simpleTest : String -> String -> Result String Music.WrittenLick -> Test
simpleTest name lick expectedResult =
    test name <|
        \_ ->
            parseLick lick
                |> Expect.equal expectedResult
