module Exercise exposing (..)

import Random.Extra
import Random.Pcg as Random exposing (Generator)
import Random.Pcg.Extra as RandomExtra
import String.Interpolate exposing (interpolate)


type alias Exercise =
    { type_ : String
    , code : String
    , answer : String
    }


plus : Generator Exercise
plus =
    Random.map2
        (\num1 num2 ->
            { type_ = "plus"
            , code =
                interpolate """
{0} + {1}
"""
                    [ toString num1
                    , toString num2
                    ]
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
                , code =
                    interpolate """
{0} - {1}
"""
                        [ toString num1
                        , toString num2
                        ]
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
                , code =
                    interpolate """
{0} - {1}
"""
                        [ toString num1
                        , toString num2
                        ]
                , answer = toString (num1 - num2)
                }
            )


times : Generator Exercise
times =
    Random.map2
        (\num1 num2 ->
            { type_ = "times"
            , code =
                interpolate """
{0} * {1}
"""
                    [ toString num1
                    , toString num2
                    ]
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
                interpolate """
let
    a = {0}
in
    a + {1}
"""
                    [ toString num1
                    , toString num2
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
                interpolate """
let
    a = {0}
    b = {1}
in
    a + b
"""
                    [ toString num1
                    , toString num2
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
                interpolate """
let
    a = {0}
    b = {1}
in
    a ++ b
"""
                    [ toString string1
                    , toString string2
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
                interpolate """
let
    a = {0}
    b = {1}
in
    a ++ b
"""
                    [ toString string1
                    , toString string2
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
                interpolate """
let
    a = {0}
    b = {1}
in
    a ++ b
"""
                    [ toString list1
                    , toString list2
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
            , code =
                interpolate """
toString {0}
"""
                    [ toString num ]
            , answer = toString (toString num)
            }
        )
        Random.Extra.smallNumber


sameNumberEquality : Generator Exercise
sameNumberEquality =
    Random.map
        (\num ->
            { type_ = "sameNumberEquality"
            , code =
                interpolate """
{0} == {0}
"""
                    [ toString num ]
            , answer = toString True
            }
        )
        Random.Extra.smallNumber


numberEquality : Generator Exercise
numberEquality =
    Random.map2
        (\num1 num2 ->
            { type_ = "numberEquality"
            , code =
                interpolate """
{0} == {1}
"""
                    [ toString num1
                    , toString num2
                    ]
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
                interpolate """
if {0} == {1} then
    {2}
else
    {3}
"""
                    [ toString num1
                    , toString num2
                    , toString num3
                    , toString num4
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
                interpolate """
if {0} /= {1} then
    {2}
else
    {3}
"""
                    [ toString num1
                    , toString num2
                    , toString num3
                    , toString num4
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
                interpolate """
if {0} < {1} then
    {2}
else
    {3}
"""
                    [ toString num1
                    , toString num2
                    , toString num3
                    , toString num4
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
                interpolate """
if {0} > {1} then
    {2}
else
    {3}
"""
                    [ toString num1
                    , toString num2
                    , toString num3
                    , toString num4
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
                interpolate """
if {0} <= {1} then
    {2}
else
    {3}
"""
                    [ toString num1
                    , toString num2
                    , toString num3
                    , toString num4
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
                interpolate """
if {0} >= {1} then
    {2}
else
    {3}
"""
                    [ toString num1
                    , toString num2
                    , toString num3
                    , toString num4
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
            , code =
                interpolate """
{0} < {1}
"""
                    [ toString num1
                    , toString num2
                    ]
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
            , code =
                interpolate """
{0} > {1}
"""
                    [ toString num1
                    , toString num2
                    ]
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
            , code =
                interpolate """
{0} <= {1}
"""
                    [ toString num1
                    , toString num2
                    ]
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
            , code =
                interpolate """
{0} >= {1}
"""
                    [ toString num1
                    , toString num2
                    ]
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
            , code =
                interpolate """
not ({0} == {1})
"""
                    [ toString num1
                    , toString num2
                    ]
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
            , code =
                interpolate """
{0} /= {1}
"""
                    [ toString num1
                    , toString num2
                    ]
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
            , code =
                interpolate """
{0} == {1}
"""
                    [ toString string1
                    , toString string2
                    ]
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
            , code =
                interpolate """
{0} /= {1}
"""
                    [ toString string1
                    , toString string2
                    ]
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
            , code =
                interpolate """
not {0}
"""
                    [ toString bool ]
            , answer = toString (not bool)
            }
        )
        Random.bool


stringLength : Generator Exercise
stringLength =
    Random.map
        (\string ->
            { type_ = "stringLength"
            , code =
                interpolate """
String.length {0}
"""
                    [ toString string ]
            , answer = toString (String.length string)
            }
        )
        (Random.Extra.lettersString 0 4)


listLength : Generator Exercise
listLength =
    Random.map
        (\list ->
            { type_ = "listLength"
            , code =
                interpolate """
List.length {0}
"""
                    [ toString list ]
            , answer = toString (List.length list)
            }
        )
        Random.Extra.smallList


stringLeftWithinLimit : Generator Exercise
stringLeftWithinLimit =
    Random.Extra.lettersString 4 6
        |> Random.andThen
            (\string ->
                Random.map2 (,)
                    (Random.constant string)
                    (Random.int 2 (String.length string - 1))
            )
        |> Random.map
            (\( string, num ) ->
                { type_ = "stringLeftWithinLimit"
                , code =
                    interpolate """
String.left {0} {1}
"""
                        [ toString num
                        , toString string
                        ]
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
                , code =
                    interpolate """
String.left {0} {1}
"""
                        [ toString num
                        , toString string
                        ]
                , answer = toString (String.left num string)
                }
            )


stringRightWithinLimit : Generator Exercise
stringRightWithinLimit =
    Random.Extra.lettersString 4 6
        |> Random.andThen
            (\string ->
                Random.map2 (,)
                    (Random.constant string)
                    (Random.int 2 (String.length string - 1))
            )
        |> Random.map
            (\( string, num ) ->
                { type_ = "stringRightWithinLimit"
                , code =
                    interpolate """
String.right {0} {1}
"""
                        [ toString num
                        , toString string
                        ]
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
                , code =
                    interpolate """
String.right {0} {1}
"""
                        [ toString num
                        , toString string
                        ]
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
                , code =
                    interpolate """
String.dropLeft {0} {1}
"""
                        [ toString num
                        , toString string
                        ]
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
                , code =
                    interpolate """
String.dropLeft {0} {1}
"""
                        [ toString num
                        , toString string
                        ]
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
                , code =
                    interpolate """
String.dropRight {0} {1}
"""
                        [ toString num
                        , toString string
                        ]
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
                , code =
                    interpolate """
String.dropRight {0} {1}
"""
                        [ toString num
                        , toString string
                        ]
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
                , code =
                    interpolate """
List.take {0} {1}
"""
                        [ toString num
                        , toString list
                        ]
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
                , code =
                    interpolate """
List.take {0} {1}
"""
                        [ toString num
                        , toString list
                        ]
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
                , code =
                    interpolate """
List.drop {0} {1}
"""
                        [ toString num
                        , toString list
                        ]
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
                , code =
                    interpolate """
List.drop {0} {1}
"""
                        [ toString num
                        , toString list
                        ]
                , answer = toString (List.drop num list)
                }
            )


function : Generator Exercise
function =
    Random.map2
        (\num1 num2 ->
            { type_ = "function"
            , code =
                interpolate """
hello a b =
    a + b

hello {0} {1}
"""
                    [ toString num1
                    , toString num2
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
                interpolate """
hello a b =
    a + b

let
    a = {0}
in
    hello a {1}
"""
                    [ toString num1
                    , toString num2
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
                interpolate """
hello a b =
    a + b

let
    a = {0}
    b = {1}
in
    hello a b
"""
                    [ toString num1
                    , toString num2
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
                interpolate """
hello a b =
    a + b

hi a b =
    a * b

hello {0} (hi {1} {2})
"""
                    [ toString num1
                    , toString num2
                    , toString num3
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
                interpolate """
hello a b =
    a + b

hi a b =
    a * b

let
    a = hi {1} {2}
in
    hello a {0}
"""
                    [ toString num1
                    , toString num2
                    , toString num3
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
                interpolate """
hello a b =
    a + b

hi a b =
    hello a (b + 1)

hi {0} {1}
"""
                    [ toString num1
                    , toString num2
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
                interpolate """
if {0} < {1} then
    {2}
else if {3} < {4} then
    {5}
else
    {6}
"""
                    [ toString num1
                    , toString num2
                    , toString num3
                    , toString num4
                    , toString num5
                    , toString num6
                    , toString num7
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
                interpolate """
hello a b =
    if a < b then
        {0}
    else
        {1}

hello {2} {3}
"""
                    [ toString num1
                    , toString num2
                    , toString num3
                    , toString num4
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
