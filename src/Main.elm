module Main exposing (main)

import Animation exposing (px)
import Color exposing (Color)
import Dom
import Exercise exposing (Exercise)
import Html as H exposing (Html)
import Html.Attributes as HA
import Html.Events as HE
import Json.Decode as JD
import Random.Extra
import Random.Pcg as Random exposing (Generator)
import Random.Pcg.Extra as RandomExtra
import Task
import Time


main : Program Never Model Msg
main =
    H.program
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


type alias Model =
    { exercises : List Exercise
    , allExercises : Int
    , answerInput : String
    , lastAttempt : Maybe Exercise
    , lastAttemptState : Animation.State
    , currentExerciseState : Animation.State
    }


type Msg
    = ExercisesGenerated (List Exercise)
    | SetAnswerInput String
    | SubmitAnswer
    | Focus (Result Dom.Error ())
    | AnimateLastAttempt Animation.Msg
    | AnimateCurrentExercise Animation.Msg
    | StartAgain


greenBgColor : Color
greenBgColor =
    Color.rgb 173 255 0


defaultBgColor : Color
defaultBgColor =
    Color.rgb 116 190 254


init : ( Model, Cmd Msg )
init =
    ( { exercises = []
      , allExercises = 0
      , answerInput = ""
      , lastAttempt = Nothing
      , lastAttemptState =
            Animation.style
                [ Animation.opacity 0
                , Animation.translate3d (px 0) (px 0) (px 0)
                ]
      , currentExerciseState =
            Animation.style
                [ Animation.backgroundColor defaultBgColor ]
      }
    , generateExercises
    )


exercises : List (Generator Exercise)
exercises =
    [ Exercise.plus
    , Exercise.minusPositive
    , Exercise.minusNegative
    , Exercise.times
    , Exercise.letAndPlus
    , Exercise.twoLetAndPlus
    , Exercise.concatStrings
    , Exercise.concatNumberStrings
    , Exercise.concatLists
    , Exercise.numberToString
    , Exercise.sameNumberEquality
    , Exercise.numberEquality
    , Exercise.numberInequality
    , Exercise.numberLessThan
    , Exercise.numberGreaterThan
    , Exercise.numberGreaterThanOrEqual
    , Exercise.numberLessThanOrEqual
    , Exercise.stringEquality
    , Exercise.stringInequality
    , Exercise.not_
    , Exercise.notNumberEquality
    , Exercise.stringLength
    , Exercise.listLength
    , Exercise.stringDropLeftWithinLimit
    , Exercise.stringDropLeftOutOfLimit
    , Exercise.stringDropRightWithinLimit
    , Exercise.stringDropRightOutOfLimit
    , Exercise.stringLeftWithinLimit
    , Exercise.stringLeftOutOfLimit
    , Exercise.stringRightWithinLimit
    , Exercise.stringRightOutOfLimit
    , Exercise.listTakeWithinLimit
    , Exercise.listTakeOutOfLimit
    , Exercise.listDropWithinLimit
    , Exercise.listDropOutOfLimit
    , Exercise.ifEqual
    , Exercise.ifNotEqual
    , Exercise.ifLessThan
    , Exercise.ifGreaterThan
    , Exercise.ifLessThanOrEqual
    , Exercise.ifGreaterThanOrEqual
    , Exercise.function
    , Exercise.letAndFunction
    , Exercise.twoLetAndFunction
    , Exercise.twoFunctionsLet
    , Exercise.twoFunctions
    , Exercise.twoFunctionsInc
    , Exercise.twoIfLessThan
    , Exercise.functionIfLessThan
    ]


generateExercises : Cmd Msg
generateExercises =
    Random.generate ExercisesGenerated
        (exercises
            |> List.map Random.Extra.three
            |> RandomExtra.combine
            |> Random.map List.concat
        )


focusCurrentAnswer : Cmd Msg
focusCurrentAnswer =
    Dom.focus "current-answer"
        |> Task.attempt Focus


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ExercisesGenerated exercises ->
            ( { model
                | exercises = exercises
                , allExercises = List.length exercises
              }
            , focusCurrentAnswer
            )

        SetAnswerInput input ->
            ( { model | answerInput = input }
            , Cmd.none
            )

        SubmitAnswer ->
            let
                lastExercise : Maybe Exercise
                lastExercise =
                    List.head model.exercises

                answerOk : Bool
                answerOk =
                    lastExercise
                        |> Maybe.map (\{ answer } -> model.answerInput == answer)
                        |> Maybe.withDefault False
            in
            ( if answerOk then
                { model
                    | exercises = model.exercises |> List.drop 1
                    , lastAttempt = Nothing
                    , answerInput = ""
                    , currentExerciseState =
                        let
                            speed =
                                Animation.easing
                                    { duration = 200 * Time.millisecond
                                    , ease = identity
                                    }
                        in
                        Animation.interrupt
                            [ Animation.toWith speed [ Animation.backgroundColor greenBgColor ]
                            , Animation.toWith speed [ Animation.backgroundColor defaultBgColor ]
                            ]
                            model.currentExerciseState
                }
              else
                { model
                    | answerInput = ""
                    , lastAttempt = lastExercise
                    , lastAttemptState =
                        let
                            speed =
                                Animation.easing
                                    { duration = 50 * Time.millisecond
                                    , ease = identity
                                    }

                            halfSpeed =
                                Animation.easing
                                    { duration = 300 * Time.millisecond
                                    , ease = identity
                                    }
                        in
                        Animation.interrupt
                            [ Animation.toWith speed [ Animation.opacity 0 ]
                            , Animation.toWith halfSpeed [ Animation.opacity 1 ]
                            , Animation.toWith speed [ Animation.translate3d (px 0) (px 0) (px 0) ]
                            , Animation.toWith speed [ Animation.translate3d (px -1) (px 0) (px 0) ]
                            , Animation.toWith speed [ Animation.translate3d (px 2) (px 0) (px 0) ]
                            , Animation.toWith speed [ Animation.translate3d (px -4) (px 0) (px 0) ]
                            , Animation.toWith speed [ Animation.translate3d (px 4) (px 0) (px 0) ]
                            , Animation.toWith speed [ Animation.translate3d (px -4) (px 0) (px 0) ]
                            , Animation.toWith speed [ Animation.translate3d (px 4) (px 0) (px 0) ]
                            , Animation.toWith speed [ Animation.translate3d (px -4) (px 0) (px 0) ]
                            , Animation.toWith speed [ Animation.translate3d (px 2) (px 0) (px 0) ]
                            , Animation.toWith speed [ Animation.translate3d (px -1) (px 0) (px 0) ]
                            , Animation.toWith speed [ Animation.translate3d (px 0) (px 0) (px 0) ]
                            , Animation.wait (500 * Time.millisecond)
                            , Animation.toWith halfSpeed [ Animation.opacity 0 ]
                            ]
                            model.lastAttemptState
                }
            , focusCurrentAnswer
            )

        Focus _ ->
            ( model
            , Cmd.none
            )

        AnimateLastAttempt animMsg ->
            ( { model | lastAttemptState = Animation.update animMsg model.lastAttemptState }
            , Cmd.none
            )

        AnimateCurrentExercise animMsg ->
            ( { model | currentExerciseState = Animation.update animMsg model.currentExerciseState }
            , Cmd.none
            )

        StartAgain ->
            ( model
            , generateExercises
            )


view : Model -> Html Msg
view model =
    H.div
        []
        {-
           -- DEBUG
           (model.exercises
               |> List.map (viewExercise model.currentExerciseState model.answerInput)
           )
        -}
        [ H.node "style" [] [ H.text """

/* Iosevka font */

@font-face {
    font-family: 'Iosevka';
    src: url('assets/iosevka-regular.woff2') format('woff2'),
         url('assets/iosevka-regular.woff') format('woff'),
         url('assets/iosevka-regular.ttf') format('truetype');
}
@font-face {
    font-family: 'Iosevka';
    font-weight: 700;
    src: url('assets/iosevka-bold.woff2') format('woff2'),
         url('assets/iosevka-bold.woff') format('woff'),
         url('assets/iosevka-bold.ttf') format('truetype');
}

/* Taken from https://css-tricks.com/css3-progress-bars/ */

.meter {
    height: 20px;  /* Can be anything */
    position: relative;
    margin: 16px 16px 20px; /* Just for demo spacing */
    background: #555;
    -moz-border-radius: 25px;
    -webkit-border-radius: 25px;
    border-radius: 25px;
    padding: 10px;
    -webkit-box-shadow: inset 0 -1px 1px rgba(255,255,255,0.3);
    -moz-box-shadow   : inset 0 -1px 1px rgba(255,255,255,0.3);
    box-shadow        : inset 0 -1px 1px rgba(255,255,255,0.3);
}
.meter > span {
    display: block;
    height: 100%;
       -webkit-border-top-right-radius: 8px;
    -webkit-border-bottom-right-radius: 8px;
           -moz-border-radius-topright: 8px;
        -moz-border-radius-bottomright: 8px;
               border-top-right-radius: 8px;
            border-bottom-right-radius: 8px;
        -webkit-border-top-left-radius: 20px;
     -webkit-border-bottom-left-radius: 20px;
            -moz-border-radius-topleft: 20px;
         -moz-border-radius-bottomleft: 20px;
                border-top-left-radius: 20px;
             border-bottom-left-radius: 20px;
    background-color: rgb(43,194,83);
    background-image: -webkit-gradient(
      linear,
      left bottom,
      left top,
      color-stop(0, rgb(43,194,83)),
      color-stop(1, rgb(84,240,84))
     );
    background-image: -moz-linear-gradient(
      center bottom,
      rgb(43,194,83) 37%,
      rgb(84,240,84) 69%
     );
    -webkit-box-shadow:
      inset 0 2px 9px  rgba(255,255,255,0.3),
      inset 0 -2px 6px rgba(0,0,0,0.4);
    -moz-box-shadow:
      inset 0 2px 9px  rgba(255,255,255,0.3),
      inset 0 -2px 6px rgba(0,0,0,0.4);
    box-shadow:
      inset 0 2px 9px  rgba(255,255,255,0.3),
      inset 0 -2px 6px rgba(0,0,0,0.4);
    position: relative;
    overflow: hidden;
}
.meter > span:after, .animate > span > span {
    content: "";
    position: absolute;
    top: 0; left: 0; bottom: 0; right: 0;
    background-image:
       -webkit-gradient(linear, 0 0, 100% 100%,
          color-stop(.25, rgba(255, 255, 255, .2)),
          color-stop(.25, transparent), color-stop(.5, transparent),
          color-stop(.5, rgba(255, 255, 255, .2)),
          color-stop(.75, rgba(255, 255, 255, .2)),
          color-stop(.75, transparent), to(transparent)
       );
    background-image:
        -moz-linear-gradient(
          -45deg,
          rgba(255, 255, 255, .2) 25%,
          transparent 25%,
          transparent 50%,
          rgba(255, 255, 255, .2) 50%,
          rgba(255, 255, 255, .2) 75%,
          transparent 75%,
          transparent
       );
    z-index: 1;
    -webkit-background-size: 50px 50px;
    -moz-background-size: 50px 50px;
    background-size: 50px 50px;
    -webkit-animation: move 2s linear infinite;
    -moz-animation: move 2s linear infinite;
       -webkit-border-top-right-radius: 8px;
    -webkit-border-bottom-right-radius: 8px;
           -moz-border-radius-topright: 8px;
        -moz-border-radius-bottomright: 8px;
               border-top-right-radius: 8px;
            border-bottom-right-radius: 8px;
        -webkit-border-top-left-radius: 20px;
     -webkit-border-bottom-left-radius: 20px;
            -moz-border-radius-topleft: 20px;
         -moz-border-radius-bottomleft: 20px;
                border-top-left-radius: 20px;
             border-bottom-left-radius: 20px;
    overflow: hidden;
}

.animate > span:after {
    display: none;
}

@-webkit-keyframes move {
    0% {
       background-position: 0 0;
    }
    100% {
       background-position: 50px 50px;
    }
}

@-moz-keyframes move {
    0% {
       background-position: 0 0;
    }
    100% {
       background-position: 50px 50px;
    }
}


.orange > span {
    background-color: #f1a165;
    background-image: -moz-linear-gradient(top, #f1a165, #f36d0a);
    background-image: -webkit-gradient(linear,left top,left bottom,color-stop(0, #f1a165),color-stop(1, #f36d0a));
    background-image: -webkit-linear-gradient(#f1a165, #f36d0a);
}

.red > span {
    background-color: #f0a3a3;
    background-image: -moz-linear-gradient(top, #f0a3a3, #f42323);
    background-image: -webkit-gradient(linear,left top,left bottom,color-stop(0, #f0a3a3),color-stop(1, #f42323));
    background-image: -webkit-linear-gradient(#f0a3a3, #f42323);
}

.nostripes > span > span, .nostripes > span:after {
    -webkit-animation: none;
    -moz-animation: none;
    background-image: none;
}
""" ]
        , H.div
            [ HA.style
                [ ( "display", "grid" )
                , ( "grid-template-columns", "repeat(3,1fr)" )
                , ( "font-size", "24px" )
                ]
            ]
            [ model.exercises
                |> List.head
                |> Maybe.map (viewExercise model.currentExerciseState model.answerInput)
                |> Maybe.withDefault viewWin
            , model.lastAttempt
                |> Maybe.map (viewLastAttempt model.lastAttemptState)
                |> Maybe.withDefault (H.text "")
            ]
        , viewProgress model.allExercises model.exercises
        ]


viewProgress : Int -> List Exercise -> Html Msg
viewProgress all currentExercises =
    let
        current : Int
        current =
            List.length currentExercises

        done : Int
        done =
            all - current

        donePercent : Float
        donePercent =
            toFloat done * 100 / toFloat all
    in
    if current == 0 then
        H.text ""
    else
        H.div []
            [ H.div
                [ HA.style
                    [ ( "font-size", "16px" )
                    , ( "font-family", "Iosevka" )
                    ]
                ]
                [ H.text "Your progress:" ]
            , H.div
                [ HA.class "meter" ]
                [ H.span
                    [ HA.style
                        [ ( "width", toString donePercent ++ "%" )
                        , ( "transition", "width 0.3s" )
                        ]
                    ]
                    []
                ]
            ]


viewWin : Html Msg
viewWin =
    H.div
        [ HA.style
            [ ( "padding", "16px 16px 32px" )
            , ( "box-shadow", "0 0 16px 0 #74bdfe" )
            , ( "margin", "16px" )
            , ( "background-color", "rgb(173,255,0)" )
            , ( "font-family", "Iosevka" )
            ]
        ]
        [ H.h1 [] [ H.text "Congratulations!" ]
        , H.h2 []
            [ H.text "You now understand the basics of the "
            , H.a
                [ HA.target "_blank"
                , HA.href "http://elm-lang.org/"
                ]
                [ H.text "Elm" ]
            , H.text " programming language!"
            ]
        , H.p []
            [ H.text "You can continue with similar games "
            , H.a
                [ HA.target "_blank"
                , HA.href "https://code.org/learn"
                ]
                [ H.text "here" ]
            , H.text " or "
            , H.a
                [ HE.onClick StartAgain
                , HA.href "#"
                ]
                [ H.text "play again!" ]
            ]
        , H.small []
            [ H.text "(This app was inspired by "
            , H.a
                [ HA.target "_blank"
                , HA.href "https://silentteacher.toxicode.fr/hourofcode"
                ]
                [ H.text "Silent Teacher" ]
            , H.text ".)"
            ]
        ]


viewExercise : Animation.State -> String -> Exercise -> Html Msg
viewExercise currentExerciseState answerInput { code } =
    H.div
        (Animation.render currentExerciseState
            ++ [ HA.style
                    [ ( "padding", "16px 16px 32px" )
                    , ( "box-shadow", "0 0 16px 0 #74bdfe" )
                    , ( "margin", "16px" )
                    ]
               ]
        )
        [ H.pre
            [ HA.style
                [ ( "font-family", "Iosevka" )
                , ( "font-weight", "bold" )
                , ( "margin", "8px 0 16px" )
                ]
            ]
            [ H.text code ]
        , H.span
            [ HA.style [ ( "font-family", "Iosevka" ) ] ]
            [ H.text "=" ]
        , H.input
            [ HE.onInput SetAnswerInput
            , onEnter SubmitAnswer
            , HA.placeholder "?"
            , HA.value answerInput
            , HA.style
                [ ( "font-family", "Iosevka" )
                , ( "margin-left", "8px" )
                , ( "font-size", "24px" )
                , ( "padding-left", "8px" )
                ]
            , HA.id "current-answer"
            ]
            []
        ]


viewLastAttempt : Animation.State -> Exercise -> Html msg
viewLastAttempt lastAttemptState { code, answer } =
    H.div
        (Animation.render lastAttemptState
            ++ [ HA.style
                    [ ( "background-color", "#98cefe" )
                    , ( "padding", "16px 16px 0" )
                    , ( "box-shadow", "0 0 16px 0 #98cefe" )
                    , ( "margin", "16px" )
                    ]
               ]
        )
        [ H.pre
            [ HA.style
                [ ( "font-family", "Iosevka" )
                , ( "font-weight", "bold" )
                , ( "margin", "8px 0 16px" )
                ]
            ]
            [ H.text code ]
        , H.pre
            [ HA.style
                [ ( "font-family", "Iosevka" )
                , ( "font-weight", "bold" )
                , ( "background-color", "#ff6633" )
                , ( "padding", "4px 8px 0" )
                ]
            ]
            [ H.text ("= " ++ answer) ]
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Animation.subscription AnimateLastAttempt [ model.lastAttemptState ]
        , Animation.subscription AnimateCurrentExercise [ model.currentExerciseState ]
        ]


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
