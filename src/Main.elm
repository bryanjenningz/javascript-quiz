module Main exposing (main)

import Browser as B
import Html as H exposing (Html)
import Html.Attributes as HA
import Html.Events as HE


type alias Model =
    { questions : List Question
    , answerShown : Bool
    , failedQuestions : List Question
    }


type alias Question =
    { question : String
    , code : String
    , answer : String
    }


init : () -> ( Model, Cmd Msg )
init () =
    ( { questions = initQuestions, answerShown = False, failedQuestions = [] }, Cmd.none )


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


view : Model -> Html Msg
view model =
    H.div
        []
        [ H.h1 [] [ H.text "JavaScript Quiz" ]
        , viewProgress model
        , case model.questions of
            [] ->
                viewQuizEnd (List.length model.failedQuestions)

            question :: _ ->
                viewQuestion model.answerShown question
        ]


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


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


main : Program () Model Msg
main =
    B.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
