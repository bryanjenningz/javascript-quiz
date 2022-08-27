module Main exposing (main)

import Browser as B
import Html as H exposing (Html)
import Html.Attributes as HA
import Html.Events as HE


type alias Model =
    { questions : List Question
    , answerShown : Bool
    }


type alias Question =
    { question : String
    , code : String
    , answer : String
    }


init : () -> ( Model, Cmd Msg )
init () =
    ( { questions = initQuestions, answerShown = False }, Cmd.none )


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
    | GoToNextQuestion
    | RetryQuiz


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ShowAnswer ->
            ( { model | answerShown = True }, Cmd.none )

        GoToNextQuestion ->
            ( { model | questions = List.drop 1 model.questions, answerShown = False }, Cmd.none )

        RetryQuiz ->
            ( { model | questions = initQuestions, answerShown = False }, Cmd.none )


view : Model -> Html Msg
view model =
    case model.questions of
        [] ->
            H.div []
                [ H.div [] [ H.text "You've finished all the questions!" ]
                , H.div [] [ H.button [ HE.onClick RetryQuiz ] [ H.text "Retry" ] ]
                ]

        question :: _ ->
            viewQuestion model.answerShown question


viewQuestion : Bool -> Question -> Html Msg
viewQuestion answerShown { question, code, answer } =
    H.div []
        [ H.div [] [ H.text question ]
        , H.pre [] [ H.text code ]
        , H.div [] [ H.textarea [ HA.placeholder "Type answer" ] [] ]
        , H.div
            [ HA.style "visibility"
                (if answerShown then
                    "visible"

                 else
                    "hidden"
                )
            ]
            [ H.div [] [ H.text "Answer:" ]
            , H.pre [] [ H.text answer ]
            ]
        , if answerShown then
            H.button [ HE.onClick GoToNextQuestion ] [ H.text "Next question" ]

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
