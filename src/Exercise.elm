module Exercise exposing (..)

import Random.Extra
import Random.Pcg as Random exposing (Generator)
import Random.Pcg.Extra as RandomExtra


type alias Exercise =
    { type_ : String
    , code : List String
    , answer : String
    }


plus : Generator Exercise
plus =
    Random.map2
        (\num1 num2 ->
            { type_ = "plus"
            , code = [ toString num1 ++ " + " ++ toString num2 ]
            , answer = toString (num1 + num2)
            }
        )
        Random.Extra.smallNumber
        Random.Extra.smallNumber


minusPositive : Generator Exercise
minusPositive =
    Random.Extra.smallNumber
        |> Random.andThen
            (\num1 ->
                Random.map2 (,)
                    (Random.int num1 (num1 + 5))
                    (Random.constant num1)
            )
        |> Random.map
            (\( num1, num2 ) ->
                { type_ = "minusPositive"
                , code = [ toString num1 ++ " - " ++ toString num2 ]
                , answer = toString (num1 - num2)
                }
            )


minusNegative : Generator Exercise
minusNegative =
    Random.Extra.smallNumber
        |> Random.andThen
            (\num1 ->
                Random.map2 (,)
                    (Random.constant num1)
                    (Random.int num1 (num1 + 5))
            )
        |> Random.map
            (\( num1, num2 ) ->
                { type_ = "minusNegative"
                , code = [ toString num1 ++ " - " ++ toString num2 ]
                , answer = toString (num1 - num2)
                }
            )


times : Generator Exercise
times =
    Random.map2
        (\num1 num2 ->
            { type_ = "times"
            , code = [ toString num1 ++ " * " ++ toString num2 ]
            , answer = toString (num1 * num2)
            }
        )
        (Random.int 1 5)
        (Random.int 1 5)


letAndPlus : Generator Exercise
letAndPlus =
    Random.map2
        (\num1 num2 ->
            { type_ = "letAndPlus"
            , code =
                [ "let"
                , "    a = " ++ toString num1
                , "in"
                , "    a + " ++ toString num2
                ]
            , answer = toString (num1 + num2)
            }
        )
        Random.Extra.smallNumber
        Random.Extra.smallNumber


twoLetAndPlus : Generator Exercise
twoLetAndPlus =
    Random.map2
        (\num1 num2 ->
            { type_ = "twoLetAndPlus"
            , code =
                [ "let"
                , "    a = " ++ toString num1
                , "    b = " ++ toString num2
                , "in"
                , "    a + b"
                ]
            , answer = toString (num1 + num2)
            }
        )
        Random.Extra.smallNumber
        Random.Extra.smallNumber


concatStrings : Generator Exercise
concatStrings =
    Random.map2
        (\string1 string2 ->
            { type_ = "concatStrings"
            , code =
                [ "let"
                , "    a = " ++ toString string1
                , "    b = " ++ toString string2
                , "in"
                , "    a ++ b"
                ]
            , answer = toString (string1 ++ string2)
            }
        )
        (Random.Extra.lettersString 0 3)
        (Random.Extra.lettersString 0 3)


concatNumberStrings : Generator Exercise
concatNumberStrings =
    Random.map2
        (\string1 string2 ->
            { type_ = "concatNumberStrings"
            , code =
                [ "let"
                , "    a = " ++ toString string1
                , "    b = " ++ toString string2
                , "in"
                , "    a ++ b"
                ]
            , answer = toString (string1 ++ string2)
            }
        )
        Random.Extra.numberString
        Random.Extra.numberString


concatLists : Generator Exercise
concatLists =
    Random.map2
        (\list1 list2 ->
            { type_ = "concatLists"
            , code =
                [ "let"
                , "    a = " ++ toString list1
                , "    b = " ++ toString list2
                , "in"
                , "    a ++ b"
                ]
            , answer = toString (list1 ++ list2)
            }
        )
        Random.Extra.smallList
        Random.Extra.smallList


numberToString : Generator Exercise
numberToString =
    Random.map
        (\num ->
            { type_ = "numberToString"
            , code = [ "toString " ++ toString num ]
            , answer = toString (toString num)
            }
        )
        Random.Extra.smallNumber


sameNumberEquality : Generator Exercise
sameNumberEquality =
    Random.map
        (\num ->
            { type_ = "sameNumberEquality"
            , code = [ toString num ++ " == " ++ toString num ]
            , answer = toString True
            }
        )
        Random.Extra.smallNumber


numberEquality : Generator Exercise
numberEquality =
    Random.map2
        (\num1 num2 ->
            { type_ = "numberEquality"
            , code = [ toString num1 ++ " == " ++ toString num2 ]
            , answer = toString (num1 == num2)
            }
        )
        Random.Extra.smallNumber
        Random.Extra.smallNumber


ifEqual : Generator Exercise
ifEqual =
    Random.map4
        (\num1 num2 num3 num4 ->
            { type_ = "ifEqual"
            , code =
                [ "if " ++ toString num1 ++ " == " ++ toString num2 ++ " then"
                , "    " ++ toString num3
                , "else"
                , "    " ++ toString num4
                ]
            , answer =
                toString
                    (if num1 == num2 then
                        num3
                     else
                        num4
                    )
            }
        )
        Random.Extra.smallNumber
        Random.Extra.smallNumber
        Random.Extra.smallNumber
        Random.Extra.smallNumber


ifNotEqual : Generator Exercise
ifNotEqual =
    Random.map4
        (\num1 num2 num3 num4 ->
            { type_ = "ifNotEqual"
            , code =
                [ "if " ++ toString num1 ++ " /= " ++ toString num2 ++ " then"
                , "    " ++ toString num3
                , "else"
                , "    " ++ toString num4
                ]
            , answer =
                toString
                    (if num1 /= num2 then
                        num3
                     else
                        num4
                    )
            }
        )
        Random.Extra.smallNumber
        Random.Extra.smallNumber
        Random.Extra.smallNumber
        Random.Extra.smallNumber


ifLessThan : Generator Exercise
ifLessThan =
    Random.map4
        (\num1 num2 num3 num4 ->
            { type_ = "ifLessThan"
            , code =
                [ "if " ++ toString num1 ++ " < " ++ toString num2 ++ " then"
                , "    " ++ toString num3
                , "else"
                , "    " ++ toString num4
                ]
            , answer =
                toString
                    (if num1 < num2 then
                        num3
                     else
                        num4
                    )
            }
        )
        Random.Extra.smallNumber
        Random.Extra.smallNumber
        Random.Extra.smallNumber
        Random.Extra.smallNumber


ifGreaterThan : Generator Exercise
ifGreaterThan =
    Random.map4
        (\num1 num2 num3 num4 ->
            { type_ = "ifGreaterThan"
            , code =
                [ "if " ++ toString num1 ++ " > " ++ toString num2 ++ " then"
                , "    " ++ toString num3
                , "else"
                , "    " ++ toString num4
                ]
            , answer =
                toString
                    (if num1 > num2 then
                        num3
                     else
                        num4
                    )
            }
        )
        Random.Extra.smallNumber
        Random.Extra.smallNumber
        Random.Extra.smallNumber
        Random.Extra.smallNumber


ifLessThanOrEqual : Generator Exercise
ifLessThanOrEqual =
    Random.map4
        (\num1 num2 num3 num4 ->
            { type_ = "ifLessThanOrEqual"
            , code =
                [ "if " ++ toString num1 ++ " <= " ++ toString num2 ++ " then"
                , "    " ++ toString num3
                , "else"
                , "    " ++ toString num4
                ]
            , answer =
                toString
                    (if num1 <= num2 then
                        num3
                     else
                        num4
                    )
            }
        )
        Random.Extra.smallNumber
        Random.Extra.smallNumber
        Random.Extra.smallNumber
        Random.Extra.smallNumber


ifGreaterThanOrEqual : Generator Exercise
ifGreaterThanOrEqual =
    Random.map4
        (\num1 num2 num3 num4 ->
            { type_ = "ifGreaterThanOrEqual"
            , code =
                [ "if " ++ toString num1 ++ " >= " ++ toString num2 ++ " then"
                , "    " ++ toString num3
                , "else"
                , "    " ++ toString num4
                ]
            , answer =
                toString
                    (if num1 >= num2 then
                        num3
                     else
                        num4
                    )
            }
        )
        Random.Extra.smallNumber
        Random.Extra.smallNumber
        Random.Extra.smallNumber
        Random.Extra.smallNumber


numberLessThan : Generator Exercise
numberLessThan =
    Random.map2
        (\num1 num2 ->
            { type_ = "numberLessThan"
            , code = [ toString num1 ++ " < " ++ toString num2 ]
            , answer = toString (num1 < num2)
            }
        )
        Random.Extra.smallNumber
        Random.Extra.smallNumber


numberGreaterThan : Generator Exercise
numberGreaterThan =
    Random.map2
        (\num1 num2 ->
            { type_ = "numberGreaterThan"
            , code = [ toString num1 ++ " > " ++ toString num2 ]
            , answer = toString (num1 > num2)
            }
        )
        Random.Extra.smallNumber
        Random.Extra.smallNumber


numberLessThanOrEqual : Generator Exercise
numberLessThanOrEqual =
    Random.map2
        (\num1 num2 ->
            { type_ = "numberLessThanOrEqual"
            , code = [ toString num1 ++ " <= " ++ toString num2 ]
            , answer = toString (num1 <= num2)
            }
        )
        Random.Extra.smallNumber
        Random.Extra.smallNumber


numberGreaterThanOrEqual : Generator Exercise
numberGreaterThanOrEqual =
    Random.map2
        (\num1 num2 ->
            { type_ = "numberGreaterThanOrEqual"
            , code = [ toString num1 ++ " >= " ++ toString num2 ]
            , answer = toString (num1 >= num2)
            }
        )
        Random.Extra.smallNumber
        Random.Extra.smallNumber


notNumberEquality : Generator Exercise
notNumberEquality =
    Random.map2
        (\num1 num2 ->
            { type_ = "notNumberEquality"
            , code = [ "not (" ++ toString num1 ++ " == " ++ toString num2 ++ ")" ]
            , answer = toString (not (num1 == num2))
            }
        )
        Random.Extra.smallNumber
        Random.Extra.smallNumber


numberInequality : Generator Exercise
numberInequality =
    Random.map2
        (\num1 num2 ->
            { type_ = "numberInequality"
            , code = [ toString num1 ++ " /= " ++ toString num2 ]
            , answer = toString (num1 /= num2)
            }
        )
        Random.Extra.smallNumber
        Random.Extra.smallNumber


stringEquality : Generator Exercise
stringEquality =
    Random.map2
        (\string1 string2 ->
            { type_ = "stringEquality"
            , code = [ toString string1 ++ " == " ++ toString string2 ]
            , answer = toString (string1 == string2)
            }
        )
        (Random.Extra.lettersString 0 4)
        (Random.Extra.lettersString 0 4)


stringInequality : Generator Exercise
stringInequality =
    Random.map2
        (\string1 string2 ->
            { type_ = "stringInequality"
            , code = [ toString string1 ++ " /= " ++ toString string2 ]
            , answer = toString (string1 /= string2)
            }
        )
        (Random.Extra.lettersString 0 4)
        (Random.Extra.lettersString 0 4)


not_ : Generator Exercise
not_ =
    Random.map
        (\bool ->
            { type_ = "bool"
            , code = [ "not " ++ toString bool ]
            , answer = toString (not bool)
            }
        )
        Random.bool


stringLength : Generator Exercise
stringLength =
    Random.map
        (\string ->
            { type_ = "stringLength"
            , code = [ "String.length " ++ toString string ]
            , answer = toString (String.length string)
            }
        )
        (Random.Extra.lettersString 0 4)


listLength : Generator Exercise
listLength =
    Random.map
        (\list ->
            { type_ = "listLength"
            , code = [ "List.length " ++ toString list ]
            , answer = toString (List.length list)
            }
        )
        Random.Extra.smallList


stringLeftWithinLimit : Generator Exercise
stringLeftWithinLimit =
    Random.Extra.lettersString 3 5
        |> Random.andThen
            (\string ->
                Random.map2 (,)
                    (Random.constant string)
                    (Random.int 1 (String.length string - 1))
            )
        |> Random.map
            (\( string, num ) ->
                { type_ = "stringLeftWithinLimit"
                , code = [ "String.left " ++ toString num ++ " " ++ toString string ]
                , answer = toString (String.left num string)
                }
            )


stringLeftOutOfLimit : Generator Exercise
stringLeftOutOfLimit =
    Random.Extra.lettersString 1 3
        |> Random.andThen
            (\string ->
                Random.map2 (,)
                    (Random.constant string)
                    (Random.int (String.length string + 1) (String.length string + 3))
            )
        |> Random.map
            (\( string, num ) ->
                { type_ = "stringLeftOutOfLimit"
                , code = [ "String.left " ++ toString num ++ " " ++ toString string ]
                , answer = toString (String.left num string)
                }
            )


stringRightWithinLimit : Generator Exercise
stringRightWithinLimit =
    Random.Extra.lettersString 3 5
        |> Random.andThen
            (\string ->
                Random.map2 (,)
                    (Random.constant string)
                    (Random.int 1 (String.length string - 1))
            )
        |> Random.map
            (\( string, num ) ->
                { type_ = "stringRightWithinLimit"
                , code = [ "String.right " ++ toString num ++ " " ++ toString string ]
                , answer = toString (String.right num string)
                }
            )


stringRightOutOfLimit : Generator Exercise
stringRightOutOfLimit =
    Random.Extra.lettersString 1 3
        |> Random.andThen
            (\string ->
                Random.map2 (,)
                    (Random.constant string)
                    (Random.int (String.length string + 1) (String.length string + 3))
            )
        |> Random.map
            (\( string, num ) ->
                { type_ = "stringRightOutOfLimit"
                , code = [ "String.right " ++ toString num ++ " " ++ toString string ]
                , answer = toString (String.right num string)
                }
            )


stringDropLeftWithinLimit : Generator Exercise
stringDropLeftWithinLimit =
    Random.Extra.lettersString 3 5
        |> Random.andThen
            (\string ->
                Random.map2 (,)
                    (Random.constant string)
                    (Random.int 1 (String.length string - 1))
            )
        |> Random.map
            (\( string, num ) ->
                { type_ = "stringDropLeftWithinLimit"
                , code = [ "String.dropLeft " ++ toString num ++ " " ++ toString string ]
                , answer = toString (String.dropLeft num string)
                }
            )


stringDropLeftOutOfLimit : Generator Exercise
stringDropLeftOutOfLimit =
    Random.Extra.lettersString 1 3
        |> Random.andThen
            (\string ->
                Random.map2 (,)
                    (Random.constant string)
                    (Random.int (String.length string + 1) (String.length string + 3))
            )
        |> Random.map
            (\( string, num ) ->
                { type_ = "stringDropLeftOutOfLimit"
                , code = [ "String.dropLeft " ++ toString num ++ " " ++ toString string ]
                , answer = toString (String.dropLeft num string)
                }
            )


stringDropRightWithinLimit : Generator Exercise
stringDropRightWithinLimit =
    Random.Extra.lettersString 3 5
        |> Random.andThen
            (\string ->
                Random.map2 (,)
                    (Random.constant string)
                    (Random.int 1 (String.length string - 1))
            )
        |> Random.map
            (\( string, num ) ->
                { type_ = "stringDropRightWithinLimit"
                , code = [ "String.dropRight " ++ toString num ++ " " ++ toString string ]
                , answer = toString (String.dropRight num string)
                }
            )


stringDropRightOutOfLimit : Generator Exercise
stringDropRightOutOfLimit =
    Random.Extra.lettersString 1 3
        |> Random.andThen
            (\string ->
                Random.map2 (,)
                    (Random.constant string)
                    (Random.int (String.length string + 1) (String.length string + 3))
            )
        |> Random.map
            (\( string, num ) ->
                { type_ = "stringDropRightOutOfLimit"
                , code = [ "String.dropRight " ++ toString num ++ " " ++ toString string ]
                , answer = toString (String.dropRight num string)
                }
            )


listTakeWithinLimit : Generator Exercise
listTakeWithinLimit =
    Random.Extra.smallList
        |> Random.andThen
            (\list ->
                Random.map2 (,)
                    (Random.constant list)
                    (Random.int 0 (List.length list))
            )
        |> Random.map
            (\( list, num ) ->
                { type_ = "listTakeWithinLimit"
                , code = [ "List.take " ++ toString num ++ " " ++ toString list ]
                , answer = toString (List.take num list)
                }
            )


listTakeOutOfLimit : Generator Exercise
listTakeOutOfLimit =
    Random.Extra.smallList
        |> Random.andThen
            (\list ->
                Random.map2 (,)
                    (Random.constant list)
                    (Random.int (List.length list + 1) (List.length list + 3))
            )
        |> Random.map
            (\( list, num ) ->
                { type_ = "listTakeOutOfLimit"
                , code = [ "List.take " ++ toString num ++ " " ++ toString list ]
                , answer = toString (List.take num list)
                }
            )


listDropWithinLimit : Generator Exercise
listDropWithinLimit =
    Random.Extra.smallList
        |> Random.andThen
            (\list ->
                Random.map2 (,)
                    (Random.constant list)
                    (Random.int 0 (List.length list))
            )
        |> Random.map
            (\( list, num ) ->
                { type_ = "listDropWithinLimit"
                , code = [ "List.drop " ++ toString num ++ " " ++ toString list ]
                , answer = toString (List.drop num list)
                }
            )


listDropOutOfLimit : Generator Exercise
listDropOutOfLimit =
    Random.Extra.smallList
        |> Random.andThen
            (\list ->
                Random.map2 (,)
                    (Random.constant list)
                    (Random.int (List.length list + 1) (List.length list + 3))
            )
        |> Random.map
            (\( list, num ) ->
                { type_ = "listDropOutOfLimit"
                , code = [ "List.drop " ++ toString num ++ " " ++ toString list ]
                , answer = toString (List.drop num list)
                }
            )


function : Generator Exercise
function =
    Random.map2
        (\num1 num2 ->
            { type_ = "function"
            , code =
                [ "hello a b ="
                , "    a + b"
                , ""
                , "hello " ++ toString num1 ++ " " ++ toString num2
                ]
            , answer = toString (num1 + num2)
            }
        )
        Random.Extra.smallNumber
        Random.Extra.smallNumber


letAndFunction : Generator Exercise
letAndFunction =
    Random.map2
        (\num1 num2 ->
            { type_ = "letAndFunction"
            , code =
                [ "hello a b ="
                , "    a + b"
                , ""
                , "let"
                , "    a = " ++ toString num1
                , "in"
                , "    hello a " ++ toString num2
                ]
            , answer = toString (num1 + num2)
            }
        )
        Random.Extra.smallNumber
        Random.Extra.smallNumber


twoLetAndFunction : Generator Exercise
twoLetAndFunction =
    Random.map2
        (\num1 num2 ->
            { type_ = "twoLetAndFunction"
            , code =
                [ "hello a b ="
                , "    a + b"
                , ""
                , "let"
                , "    a = " ++ toString num1
                , "    b = " ++ toString num2
                , "in"
                , "    hello a b"
                ]
            , answer = toString (num1 + num2)
            }
        )
        Random.Extra.smallNumber
        Random.Extra.smallNumber


twoFunctions : Generator Exercise
twoFunctions =
    Random.map3
        (\num1 num2 num3 ->
            { type_ = "twoFunctions"
            , code =
                [ "hello a b ="
                , "    a + b"
                , ""
                , "hi a b ="
                , "    a * b"
                , ""
                , "hello " ++ toString num1 ++ " (hi " ++ toString num2 ++ " " ++ toString num3 ++ ")"
                ]
            , answer = toString (num1 + num2 * num3)
            }
        )
        Random.Extra.extraSmallNumber
        Random.Extra.extraSmallNumber
        Random.Extra.extraSmallNumber


twoFunctionsLet : Generator Exercise
twoFunctionsLet =
    Random.map3
        (\num1 num2 num3 ->
            { type_ = "twoFunctionsLet"
            , code =
                [ "hello a b ="
                , "    a + b"
                , ""
                , "hi a b ="
                , "    a * b"
                , ""
                , "let"
                , "    a = hi " ++ toString num2 ++ " " ++ toString num3
                , "in"
                , "    hello a " ++ toString num1
                ]
            , answer = toString (num1 + num2 * num3)
            }
        )
        Random.Extra.extraSmallNumber
        Random.Extra.extraSmallNumber
        Random.Extra.extraSmallNumber


twoFunctionsInc : Generator Exercise
twoFunctionsInc =
    Random.map2
        (\num1 num2 ->
            { type_ = "twoFunctionsInc"
            , code =
                [ "hello a b ="
                , "    a + b"
                , ""
                , "hi a b ="
                , "    hello a (b + 1)"
                , ""
                , "hi " ++ toString num1 ++ " " ++ toString num2
                ]
            , answer = toString (num1 + num2 + 1)
            }
        )
        Random.Extra.extraSmallNumber
        Random.Extra.extraSmallNumber


twoIfLessThan : Generator Exercise
twoIfLessThan =
    Random.map
        (\num1 num2 num3 num4 num5 num6 num7 ->
            { type_ = "twoIfLessThan"
            , code =
                [ "if " ++ toString num1 ++ " < " ++ toString num2 ++ " then"
                , "    " ++ toString num3
                , "else if " ++ toString num4 ++ " < " ++ toString num5 ++ " then"
                , "    " ++ toString num6
                , "else"
                , "    " ++ toString num7
                ]
            , answer =
                toString
                    (if num1 < num2 then
                        num3
                     else if num4 < num5 then
                        num6
                     else
                        num7
                    )
            }
        )
        Random.Extra.smallNumber
        |> RandomExtra.andMap Random.Extra.smallNumber
        |> RandomExtra.andMap Random.Extra.smallNumber
        |> RandomExtra.andMap Random.Extra.smallNumber
        |> RandomExtra.andMap Random.Extra.smallNumber
        |> RandomExtra.andMap Random.Extra.smallNumber
        |> RandomExtra.andMap Random.Extra.smallNumber


functionIfLessThan : Generator Exercise
functionIfLessThan =
    Random.map4
        (\num1 num2 num3 num4 ->
            { type_ = "functionIfLessThan"
            , code =
                [ "hello a b ="
                , "    if a < b then"
                , "        " ++ toString num1
                , "    else"
                , "        " ++ toString num2
                , ""
                , "hello " ++ toString num3 ++ " " ++ toString num4
                ]
            , answer =
                toString
                    (if num3 < num4 then
                        num1
                     else
                        num2
                    )
            }
        )
        Random.Extra.smallNumber
        Random.Extra.smallNumber
        Random.Extra.smallNumber
        Random.Extra.smallNumber
