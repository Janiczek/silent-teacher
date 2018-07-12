module Main exposing (main)

import Animation exposing (px)
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
    , answerInput : String
    , lastAttempt : Maybe Exercise
    , lastAttemptState : Animation.State
    }


type Msg
    = ExercisesGenerated (List Exercise)
    | SetAnswerInput String
    | SubmitAnswer
    | Focus (Result Dom.Error ())
    | AnimateLastAttempt Animation.Msg


init : ( Model, Cmd Msg )
init =
    ( { exercises = []
      , answerInput = ""
      , lastAttempt = Nothing
      , lastAttemptState =
            Animation.style
                [ Animation.opacity 0
                , Animation.translate3d (px 0) (px 0) (px 0)
                ]
      }
    , Random.generate ExercisesGenerated
        ([ Exercise.plus
         , Exercise.minus
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
         , Exercise.stringDropLeft
         , Exercise.stringDropRight
         , Exercise.stringLeft
         , Exercise.stringRight
         , Exercise.listTake
         , Exercise.listDrop
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
            |> List.map Random.Extra.three
            |> RandomExtra.combine
            |> Random.map List.concat
        )
    )


focusCurrentAnswer : Cmd Msg
focusCurrentAnswer =
    Dom.focus "current-answer"
        |> Task.attempt Focus


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ExercisesGenerated exercises ->
            ( { model | exercises = exercises }
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
                            , Animation.wait (1000 * Time.millisecond)
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


view : Model -> Html Msg
view model =
    H.div
        [ HA.style
            [ ( "display", "grid" )
            , ( "grid-template-columns", "repeat(3,1fr)" )
            , ( "font-size", "24px" )
            ]
        ]
        [ model.exercises
            |> List.head
            |> Maybe.map (viewExercise model.answerInput)
            |> Maybe.withDefault (H.text "")
        , model.lastAttempt
            |> Maybe.map (viewLastAttempt model.lastAttemptState)
            |> Maybe.withDefault (H.text "")
        ]


viewExercise : String -> Exercise -> Html Msg
viewExercise answerInput { code } =
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
                    , ( "padding", "16px" )
                    , ( "box-shadow", "0 0 16px 0 #98cefe" )
                    , ( "margin", "16px" )
                    ]
               ]
        )
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


subscriptions : Model -> Sub Msg
subscriptions model =
    Animation.subscription AnimateLastAttempt [ model.lastAttemptState ]


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
