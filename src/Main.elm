module Main exposing (main)

import Browser as B
import Html as H exposing (Html)
import Html.Attributes as HA
import Html.Events as HE
import Task
import Time


type alias Model =
    { currentQuiz : Maybe Quiz
    , quizzes : List Quiz
    }


type alias Quiz =
    { title : String
    , questions : List Question
    , answerShown : Bool
    , failedQuestions : List Question
    , startTime : Maybe Int
    , nowTime : Maybe Int
    , totalQuestions : Int
    }


type alias Question =
    { question : String
    , code : String
    , answer : String
    }


init : () -> ( Model, Cmd Msg )
init () =
    ( { currentQuiz = Nothing
      , quizzes =
            List.map initQuiz
                [ ( "\"this\" quiz", thisQuestions )
                , ( "Promise quiz", promiseQuestions )
                ]
      }
    , Cmd.map QuizMsg setStartAndNowTime
    )


initQuiz : ( String, List Question ) -> Quiz
initQuiz ( title, questions ) =
    { title = title
    , questions = questions
    , answerShown = False
    , failedQuestions = []
    , startTime = Nothing
    , nowTime = Nothing
    , totalQuestions = List.length questions
    }


thisQuestions : List Question
thisQuestions =
    [ { question = "What is passed in as the \"this\" argument?"
      , code = "a.f(b)"
      , answer = "a"
      }
    , { question = "What is passed in as the \"this\" argument?"
      , code = "new f(a)"
      , answer = "an empty object which has f.prototype as its prototype"
      }
    , { question = "What is passed in as the \"this\" argument?"
      , code = "f(a)"
      , answer = "window in browsers, global in node.js, or undefined in strict mode"
      }
    , { question = "What is passed in as the \"this\" argument?"
      , code = "f.call(a, b, c)"
      , answer = "a"
      }
    , { question = "What is passed in as the \"this\" argument?"
      , code = "f.apply(a, [b, c])"
      , answer = "a"
      }
    , { question = "What is passed in as the \"this\" argument?"
      , code = "f.bind(a, b)(c)"
      , answer = "a"
      }
    , { question = "What is passed in as the \"this\" argument?"
      , code = "new o.f(a)"
      , answer = "an empty object which has f.prototype as its prototype"
      }
    ]


promiseQuestions : List Question
promiseQuestions =
    [ { question = "What gets logged to the console?"
      , code = """new Promise(resolve => {
  console.log(1)
  resolve()
})
.then(() => console.log(2))
console.log(3)"""
      , answer = "1 3 2"
      }
    , { question = "What gets logged to the console?"
      , code = """new Promise(resolve => {
  console.log(1)
  new Promise(resolve => {
    console.log(2)
    resolve()
  })
  .then(() => console.log(3))
  resolve()
  console.log(4)
})
.then(() => console.log(5))
console.log(6)"""
      , answer = "1 2 4 6 3 5"
      }
    ]


setStartAndNowTime : Cmd QuizMsg
setStartAndNowTime =
    Task.perform (Time.posixToMillis >> SetStartAndNowTime) Time.now


type Msg
    = StartQuiz Quiz
    | QuizMsg QuizMsg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        StartQuiz startedQuiz ->
            ( { model
                | currentQuiz =
                    model.quizzes
                        |> List.filter (\quiz -> quiz.title == startedQuiz.title)
                        |> List.head
              }
            , Cmd.map QuizMsg setStartAndNowTime
            )

        QuizMsg quizMsg ->
            case model.currentQuiz of
                Nothing ->
                    ( model, Cmd.none )

                Just quiz ->
                    case quizMsg of
                        EndQuiz ->
                            ( { model | currentQuiz = Nothing }, Cmd.none )

                        _ ->
                            let
                                ( newQuiz, quizCmd ) =
                                    updateQuiz quizMsg quiz
                            in
                            ( { model | currentQuiz = Just newQuiz }
                            , Cmd.map QuizMsg quizCmd
                            )


type QuizMsg
    = ShowAnswer
    | PassQuestion
    | FailQuestion
    | RetryFailedQuestions
    | SetStartAndNowTime Int
    | SetNowTime Int
    | EndQuiz


updateQuiz : QuizMsg -> Quiz -> ( Quiz, Cmd QuizMsg )
updateQuiz msg quiz =
    case msg of
        ShowAnswer ->
            ( { quiz | answerShown = True }, Cmd.none )

        PassQuestion ->
            ( { quiz | questions = List.drop 1 quiz.questions, answerShown = False }
            , Cmd.none
            )

        FailQuestion ->
            ( { quiz
                | questions = List.drop 1 quiz.questions
                , answerShown = False
                , failedQuestions = quiz.failedQuestions ++ List.take 1 quiz.questions
              }
            , Cmd.none
            )

        RetryFailedQuestions ->
            ( { quiz
                | questions = quiz.failedQuestions
                , answerShown = False
                , failedQuestions = []
              }
            , setStartAndNowTime
            )

        SetStartAndNowTime time ->
            ( { quiz | startTime = Just time, nowTime = Just time }, Cmd.none )

        SetNowTime time ->
            ( { quiz | nowTime = Just time }, Cmd.none )

        EndQuiz ->
            -- Gets handled at above level
            ( quiz, Cmd.none )


view : Model -> Html Msg
view model =
    case model.currentQuiz of
        Nothing ->
            H.div []
                [ H.h1 [] [ H.text "Choose a JavaScript quiz" ]
                , H.div [] (List.map viewStartQuizButton model.quizzes)
                ]

        Just currentQuiz ->
            H.map QuizMsg (viewQuiz currentQuiz)


viewStartQuizButton : Quiz -> Html Msg
viewStartQuizButton quiz =
    H.button
        [ HA.style "display" "block"
        , HA.style "margin" "10px auto"
        , HE.onClick (StartQuiz quiz)
        ]
        [ H.text quiz.title ]


viewQuiz : Quiz -> Html QuizMsg
viewQuiz quiz =
    H.div
        []
        [ H.h1 [] [ H.text quiz.title ]
        , viewTime quiz
        , viewProgress quiz
        , case quiz.questions of
            [] ->
                viewQuizEnd quiz.failedQuestions

            question :: _ ->
                viewQuestion quiz.answerShown question
        ]


viewTime : { a | startTime : Maybe Int, nowTime : Maybe Int } -> Html msg
viewTime { startTime, nowTime } =
    case ( startTime, nowTime ) of
        ( Just start, Just now ) ->
            let
                totalSeconds =
                    (now - start) // 1000

                seconds =
                    totalSeconds |> modBy 60

                minutes =
                    totalSeconds // 60

                formattedTime =
                    String.padLeft 2 '0' (String.fromInt minutes)
                        ++ ":"
                        ++ String.padLeft 2 '0' (String.fromInt seconds)
            in
            H.div [] [ H.text formattedTime ]

        _ ->
            H.div [] [ H.text "00:00" ]


viewProgress : Quiz -> Html msg
viewProgress { questions, totalQuestions } =
    let
        completed =
            totalQuestions - List.length questions
    in
    H.label []
        [ H.p []
            [ H.text
                ("Completed "
                    ++ String.fromInt completed
                    ++ " out of "
                    ++ String.fromInt totalQuestions
                )
            ]
        , H.progress
            [ HA.max (String.fromInt totalQuestions)
            , HA.value (String.fromInt completed)
            ]
            []
        ]


viewQuizEnd : List Question -> Html QuizMsg
viewQuizEnd failedQuestions =
    H.div []
        [ H.div [ HA.style "margin" "10px 0" ]
            [ H.text "You've finished all the questions!" ]
        , case List.length failedQuestions of
            0 ->
                H.div []
                    [ H.div [ HA.style "margin" "10px 0" ]
                        [ H.text "Congratulations! No failed questions!" ]
                    , H.button [ HE.onClick EndQuiz ] [ H.text "Go back" ]
                    ]

            failedQuestionCount ->
                H.div []
                    [ H.div [ HA.style "margin" "10px 0" ]
                        [ H.text
                            ("You failed "
                                ++ String.fromInt failedQuestionCount
                                ++ (if failedQuestionCount == 1 then
                                        " question."

                                    else
                                        " questions."
                                   )
                            )
                        ]
                    , H.button
                        [ HE.onClick RetryFailedQuestions ]
                        [ H.text "Retry failed questions" ]
                    , viewFailedQuestions failedQuestions
                    ]
        ]


viewQuestion : Bool -> Question -> Html QuizMsg
viewQuestion answerShown { question, code, answer } =
    H.div []
        [ H.h2 [] [ H.text question ]
        , H.pre [] [ H.text code ]
        , H.div
            [ HA.style "visibility"
                (if answerShown then
                    "visible"

                 else
                    "hidden"
                )
            ]
            [ H.h2 [] [ H.text "Answer:" ]
            , H.pre [] [ H.text answer ]
            ]
        , if answerShown then
            H.span []
                [ H.button [ HE.onClick FailQuestion ] [ H.text "Fail" ]
                , H.button [ HE.onClick PassQuestion ] [ H.text "Pass" ]
                ]

          else
            H.button [ HE.onClick ShowAnswer ] [ H.text "Show answer" ]
        ]


viewFailedQuestions : List Question -> Html msg
viewFailedQuestions questions =
    case questions of
        [] ->
            H.text ""

        _ ->
            H.div []
                [ H.h2 [] [ H.text "Failed questions" ]
                , H.div [] (List.map viewFailedQuestion questions)
                ]


viewFailedQuestion : Question -> Html msg
viewFailedQuestion { question, code, answer } =
    H.div
        [ HA.style "margin" "10px auto"
        , HA.style "padding" "10px"
        , HA.style "background-color" "#000"
        ]
        [ H.div [] [ H.text question ]
        , H.pre [] [ H.text code ]
        , H.div [] [ H.text ("Answer: " ++ answer) ]
        ]


timeInterval : Float
timeInterval =
    1000


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.currentQuiz of
        Nothing ->
            Sub.none

        Just currentQuiz ->
            if List.isEmpty currentQuiz.questions then
                Sub.none

            else
                Time.every timeInterval
                    (QuizMsg << SetNowTime << Time.posixToMillis)


main : Program () Model Msg
main =
    B.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
