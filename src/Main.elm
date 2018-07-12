module Main exposing (main)

import Dom
import Exercise exposing (Exercise)
import Html as H exposing (Html)
import Html.Attributes as HA
import Random.Extra
import Random.Pcg as Random exposing (Generator)
import Random.Pcg.Extra as RandomExtra
import Task


main : Program Never Model Msg
main =
    H.program
        { init = init
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
        }


type alias Model =
    { exercises : List Exercise
    , answerInput : String
    , lastAttempt : Maybe Exercise
    }


type Msg
    = ExercisesGenerated (List Exercise)
    | SetAnswerInput String
    | SubmitAnswer
    | Focus (Result Dom.Error ())


init : ( Model, Cmd Msg )
init =
    ( { exercises = []
      , answerInput = ""
      , lastAttempt = Nothing
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
                }
            , focusCurrentAnswer
            )

        Focus _ ->
            ( model, Cmd.none )


view : Model -> Html Msg
view model =
    H.div
        [ HA.style
            [ ( "display", "grid" )
            , ( "grid-template-columns", "repeat(3,1fr)" )
            , ( "font-size", "24px" )
            ]
        ]
        [ H.node "style" [] [ H.text """
@keyframes shake {
    0% { opacity: 0; }
  1%, 9% {
    transform: translate3d(-1px, 0, 0);
  }

  2%, 8% {
    transform: translate3d(2px, 0, 0);
  }

  3%, 5%, 7% {
    transform: translate3d(-4px, 0, 0);
  }

  4%, 6% {
    transform: translate3d(4px, 0, 0);
  }
    10%, 30% { opacity: 1; }
    60%, 100% {opacity: 0;}
}
        """ ]
        , model.exercises
            |> List.head
            |> Maybe.map
                (Exercise.view
                    { onInput = SetAnswerInput
                    , onSubmit = SubmitAnswer
                    , value = model.answerInput
                    }
                )
            |> Maybe.withDefault (H.text "")
        , model.lastAttempt
            |> Maybe.map Exercise.viewLastAttempt
            |> Maybe.withDefault (H.text "")
        ]
