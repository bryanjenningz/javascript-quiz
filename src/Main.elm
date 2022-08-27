module Main exposing (main)

import Browser as B
import Html as H exposing (Html)
import Html.Attributes as HA
import Html.Events as HE
import Task
import Time


type alias Model =
    { questions : List Question
    , answerShown : Bool
    , failedQuestions : List Question
    , startTime : Maybe Int
    , nowTime : Maybe Int
    }


type alias Question =
    { question : String
    , code : String
    , answer : String
    }


init : () -> ( Model, Cmd Msg )
init () =
    ( { questions = initQuestions
      , answerShown = False
      , failedQuestions = []
      , startTime = Nothing
      , nowTime = Nothing
      }
    , Task.perform (Time.posixToMillis >> SetStartTime) Time.now
    )


initQuestions : List Question
initQuestions =
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
      , answer = "window in browsers, global in node.js, undefined in strict mode"
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


type Msg
    = ShowAnswer
    | PassQuestion
    | FailQuestion
    | RetryFailedQuestions
    | SetStartTime Int
    | SetNowTime Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ShowAnswer ->
            ( { model | answerShown = True }, Cmd.none )

        PassQuestion ->
            ( { model | questions = List.drop 1 model.questions, answerShown = False }
            , Cmd.none
            )

        FailQuestion ->
            ( { model
                | questions = List.drop 1 model.questions
                , answerShown = False
                , failedQuestions = model.failedQuestions ++ List.take 1 model.questions
              }
            , Cmd.none
            )

        RetryFailedQuestions ->
            ( { model
                | questions = model.failedQuestions
                , answerShown = False
                , failedQuestions = []
              }
            , Cmd.none
            )

        SetStartTime time ->
            ( { model | startTime = Just time }, Cmd.none )

        SetNowTime time ->
            ( { model | nowTime = Just time }, Cmd.none )


view : Model -> Html Msg
view model =
    H.div
        []
        [ H.h1 [] [ H.text "JavaScript Quiz" ]
        , viewTime model
        , viewProgress model
        , case model.questions of
            [] ->
                viewQuizEnd (List.length model.failedQuestions)

            question :: _ ->
                viewQuestion model.answerShown question
        ]


viewTime : Model -> Html msg
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


viewProgress : Model -> Html msg
viewProgress model =
    let
        total =
            List.length initQuestions

        completed =
            total - List.length model.questions
    in
    H.label []
        [ H.p []
            [ H.text
                ("Completed "
                    ++ String.fromInt completed
                    ++ " out of "
                    ++ String.fromInt total
                )
            ]
        , H.progress
            [ HA.max (String.fromInt total)
            , HA.value (String.fromInt completed)
            ]
            []
        ]


viewQuizEnd : Int -> Html Msg
viewQuizEnd failedQuestionsCount =
    H.div []
        [ H.div [] [ H.text "You've finished all the questions!" ]
        , H.div []
            [ if failedQuestionsCount > 0 then
                H.div []
                    [ H.div []
                        [ H.text
                            ("You failed "
                                ++ String.fromInt failedQuestionsCount
                                ++ (if failedQuestionsCount == 1 then
                                        " question."

                                    else
                                        " questions."
                                   )
                            )
                        ]
                    , H.button
                        [ HE.onClick RetryFailedQuestions ]
                        [ H.text "Retry failed questions" ]
                    ]

              else
                H.text "Congratulations! No failed questions!"
            ]
        ]


viewQuestion : Bool -> Question -> Html Msg
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


timeInterval : Float
timeInterval =
    1000


subscriptions : Model -> Sub Msg
subscriptions model =
    if List.isEmpty model.questions then
        Sub.none

    else
        Time.every timeInterval (Time.posixToMillis >> SetNowTime)


main : Program () Model Msg
main =
    B.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
