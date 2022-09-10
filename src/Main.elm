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
    , setStartAndNowTime
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


setStartAndNowTime : Cmd Msg
setStartAndNowTime =
    Task.perform (Time.posixToMillis >> SetStartAndNowTime) Time.now


type Msg
    = ShowAnswer
    | PassQuestion
    | FailQuestion
    | RetryFailedQuestions
    | SetStartAndNowTime Int
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
            , setStartAndNowTime
            )

        SetStartAndNowTime time ->
            ( { model | startTime = Just time, nowTime = Just time }, Cmd.none )

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
                viewQuizEnd model.failedQuestions

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


viewQuizEnd : List Question -> Html Msg
viewQuizEnd failedQuestions =
    H.div []
        [ H.div [] [ H.text "You've finished all the questions!" ]
        , H.div []
            [ if List.length failedQuestions > 0 then
                H.div []
                    [ H.div []
                        [ H.text
                            ("You failed "
                                ++ String.fromInt (List.length failedQuestions)
                                ++ (if List.length failedQuestions == 1 then
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


viewFailedQuestions : List Question -> Html Msg
viewFailedQuestions questions =
    case questions of
        [] ->
            H.text ""

        _ ->
            H.div []
                [ H.h2 [] [ H.text "Failed questions" ]
                , H.div [] (List.map viewFailedQuestion questions)
                ]


viewFailedQuestion : Question -> Html Msg
viewFailedQuestion { question, code, answer } =
    H.div
        [ HA.style "margin" "10px auto"
        , HA.style "padding" "10px"
        , HA.style "background-color" "#000"
        ]
        [ H.div [] [ H.text question ]
        , H.div [] [ H.text code ]
        , H.div [] [ H.text ("Answer: " ++ answer) ]
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
