module MusicTest exposing (all)

import Music exposing (parseLick)
import Expect
import Test exposing (Test, describe, test)


all: Test
all = 
  describe "All tests regarding the Music module"
  [ lickParser ]


lickParser: Test
lickParser = 
  describe "Test lib parsing" 
  [ simpleLickParsing ]


simpleLickParsing: Test
simpleLickParsing = 
  describe "Simple examples of parsing" [ 
    simpleTest "Empty lick" "" []
    , simpleTest "simple single note" "4" [ Music.HarmonicaNote Music.B4 ]
    , simpleTest "single draw note" "-4" [ Music.HarmonicaNote Music.D4 ]
    , simpleTest "with a bend" "-3'" [ Music.HarmInterstitialNote Music.D3b]
    , simpleTest "only gibberish" "whatever" [ Music.Annotation "whatever" ]
    ]


simpleTest: String ->  String -> WrittenLick -> Test
simpleTest name lick expectedResult = 
  test name <| \_ -> parseLick lick 
    |> Expect.equal expectedResult

