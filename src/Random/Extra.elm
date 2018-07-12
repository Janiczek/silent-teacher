module Random.Extra exposing (..)

import Random.Pcg as Random exposing (Generator)
import Random.Pcg.Char as RandomChar
import Random.Pcg.Extra as RandomExtra
import Random.Pcg.String as RandomString


three : Generator a -> Generator (List a)
three generator =
    Random.map3 (\a b c -> [ a, b, c ])
        generator
        generator
        generator


numberChar : Generator Char
numberChar =
    RandomChar.char 48 57


smallNumber : Generator Int
smallNumber =
    Random.int 0 9


extraSmallNumber : Generator Int
extraSmallNumber =
    Random.int 0 4


lettersString : Generator String
lettersString =
    RandomString.rangeLengthString 2 4 RandomChar.lowerCaseLatin


possiblyEmptyLettersString : Generator String
possiblyEmptyLettersString =
    RandomString.rangeLengthString 0 4 RandomChar.lowerCaseLatin


numberString : Generator String
numberString =
    RandomString.rangeLengthString 1 3 numberChar


smallList : Generator (List Int)
smallList =
    RandomExtra.rangeLengthList 0 4 smallNumber
