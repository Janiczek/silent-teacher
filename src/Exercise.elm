module Exercise exposing (..)

import Html as H exposing (Html)
import Html.Attributes as HA
import Html.Events as HE
import Json.Decode as JD
import Random.Extra
import Random.Pcg as Random exposing (Generator)
import Random.Pcg.Extra as RandomExtra


type alias Exercise =
    { type_ : String
    , code : List String
    , answer : String
    }


type alias Config msg =
    { onInput : String -> msg
    , onSubmit : msg
    , value : String
    }


view : Config msg -> Exercise -> Html msg
view { onInput, onSubmit, value } { code } =
    H.div
        [ HA.style
            [ ( "background-color", "#74bdfe" )
            , ( "padding", "16px" )
            , ( "box-shadow", "0 0 16px 0 #74bdfe" )
            , ( "margin", "16px" )
            ]
        ]
        [ H.pre
            [ HA.style
                [ ( "font-family", "Iosevka" )
                , ( "font-weight", "bold" )
                ]
            ]
            [ H.text (code |> String.join "\n") ]
        , H.text "="
        , H.input
            [ HE.onInput onInput
            , onEnter onSubmit
            , HA.placeholder "?"
            , HA.value value
            , HA.style
                [ ( "font-family", "Iosevka" )
                , ( "margin-left", "8px" )
                , ( "font-size", "24px" )
                ]
            , HA.id "current-answer"
            ]
            []
        ]


viewLastAttempt : Exercise -> Html msg
viewLastAttempt { code, answer } =
    H.div
        [ HA.style
            [ ( "background-color", "#98cefe" )
            , ( "padding", "16px" )
            , ( "box-shadow", "0 0 16px 0 #98cefe" )
            , ( "margin", "16px" )
            , ( "animation", "shake 5s" )
            , ( "animation-iteration-count", "1" )
            , ( "animation-fill-mode", "forwards" )
            ]
        ]
        [ H.pre
            [ HA.style
                [ ( "font-family", "Iosevka" )
                , ( "font-weight", "bold" )
                ]
            ]
            [ H.text (code |> String.join "\n") ]
        , H.pre
            [ HA.style
                [ ( "font-family", "Iosevka" )
                , ( "font-weight", "bold" )
                , ( "background-color", "rgba(255,0,0,0.3)" )
                , ( "padding", "8px" )
                ]
            ]
            [ H.text ("= " ++ answer) ]
        ]


viewDebug : Exercise -> Html msg
viewDebug { type_, code, answer } =
    H.div
        [ HA.style
            [ ( "background-color", "#74bdfe" )
            , ( "padding", "16px" )
            , ( "box-shadow", "0 0 16px 0 #74bdfe" )
            , ( "margin", "16px" )
            ]
        ]
        [ H.strong
            [ HA.style [ ( "color", "#fff" ) ] ]
            [ H.text type_ ]
        , H.pre
            [ HA.style
                [ ( "font-family", "Iosevka" )
                , ( "font-weight", "bold" )
                ]
            ]
            [ H.text (code |> String.join "\n") ]
        , H.strong
            [ HA.style [ ( "color", "#fff" ) ] ]
            [ H.text "Answer:" ]
        , H.pre
            [ HA.style
                [ ( "font-family", "Iosevka" )
                , ( "color", "#fff" )
                ]
            ]
            [ H.text answer ]
        ]


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


minus : Generator Exercise
minus =
    Random.map2
        (\num1 num2 ->
            { type_ = "minus"
            , code = [ toString num1 ++ " - " ++ toString num2 ]
            , answer = toString (num1 - num2)
            }
        )
        Random.Extra.smallNumber
        Random.Extra.smallNumber


times : Generator Exercise
times =
    Random.map2
        (\num1 num2 ->
            { type_ = "times"
            , code = [ toString num1 ++ " * " ++ toString num2 ]
            , answer = toString (num1 * num2)
            }
        )
        Random.Extra.smallNumber
        Random.Extra.smallNumber


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
        Random.Extra.lettersString
        Random.Extra.lettersString


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
        Random.Extra.possiblyEmptyLettersString
        Random.Extra.possiblyEmptyLettersString


stringInequality : Generator Exercise
stringInequality =
    Random.map2
        (\string1 string2 ->
            { type_ = "stringInequality"
            , code = [ toString string1 ++ " /= " ++ toString string2 ]
            , answer = toString (string1 /= string2)
            }
        )
        Random.Extra.possiblyEmptyLettersString
        Random.Extra.possiblyEmptyLettersString


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
        Random.Extra.possiblyEmptyLettersString


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


stringLeft : Generator Exercise
stringLeft =
    Random.map2
        (\string num ->
            { type_ = "stringLeft"
            , code = [ "String.left " ++ toString num ++ " " ++ toString string ]
            , answer = toString (String.left num string)
            }
        )
        Random.Extra.lettersString
        Random.Extra.extraSmallNumber


stringRight : Generator Exercise
stringRight =
    Random.map2
        (\string num ->
            { type_ = "stringRight"
            , code = [ "String.right " ++ toString num ++ " " ++ toString string ]
            , answer = toString (String.right num string)
            }
        )
        Random.Extra.lettersString
        Random.Extra.extraSmallNumber


stringDropLeft : Generator Exercise
stringDropLeft =
    Random.map2
        (\string num ->
            { type_ = "stringDropLeft"
            , code = [ "String.dropLeft " ++ toString num ++ " " ++ toString string ]
            , answer = toString (String.dropLeft num string)
            }
        )
        Random.Extra.lettersString
        Random.Extra.extraSmallNumber


stringDropRight : Generator Exercise
stringDropRight =
    Random.map2
        (\string num ->
            { type_ = "stringDropRight"
            , code = [ "String.dropRight " ++ toString num ++ " " ++ toString string ]
            , answer = toString (String.dropRight num string)
            }
        )
        Random.Extra.lettersString
        Random.Extra.extraSmallNumber


listTake : Generator Exercise
listTake =
    Random.map2
        (\list num ->
            { type_ = "listTake"
            , code = [ "List.take " ++ toString num ++ " " ++ toString list ]
            , answer = toString (List.take num list)
            }
        )
        Random.Extra.smallList
        Random.Extra.extraSmallNumber


listDrop : Generator Exercise
listDrop =
    Random.map2
        (\list num ->
            { type_ = "listDrop"
            , code = [ "List.drop " ++ toString num ++ " " ++ toString list ]
            , answer = toString (List.drop num list)
            }
        )
        Random.Extra.smallList
        Random.Extra.extraSmallNumber


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


{-| When the enter key is released, send the `msg`.
Otherwise, do nothing.
Taken from elm-community/html-extra
-}
onEnter : msg -> H.Attribute msg
onEnter onEnterAction =
    HE.on "keyup" <|
        JD.andThen
            (\keyCode ->
                if keyCode == 13 then
                    JD.succeed onEnterAction
                else
                    JD.fail (toString keyCode)
            )
            HE.keyCode
