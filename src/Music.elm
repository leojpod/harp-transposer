module Music exposing (..)

type Chromatic = 
    -- NOTE: this needs to be between 0 and 12, should it be checked at compile time?
    -- C0 | C1 | C2 | C3 | C4 | C5 | C6
    -- | C7 | C8 | C9 | C10 | C11
    Int


type HarmonicaNote = 
    B1 | D1 | D1b
    | B2 | D2 | D2b | D2bb
    | B3 | D3 | D3b | D3bb | D3bbb
    | B4o | B4 | D4 | D4b
    | B5o | D5
    | B6o | B6 | D6 | D6b
    | B7 | D7 | D7b 
    | B8b | B8 | D8
    | B9b | B9 | D9 | D9o
    | B10bb | B10b | B10 | D10 | D10o


type alias Position = 
   { to: HarmonicaNote -> Chromatic
   , from: Chromatic -> Maybe HarmonicaNote}


type WrittenLick = 
    List LickElement

type LickElement = 
    Annotation String
    | HarmonicaNote HarmonicaNote



parseLick: String -> WrittenLick
parseLick = Debug.todo "parse lick"
