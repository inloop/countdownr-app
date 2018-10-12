module Main exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import Iso8601
import Task
import Time exposing (..)
import TimeFormatter exposing (calcDeadline, posixMidnight)
import Url.Builder as Url



-- MAIN


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }



-- MODEL


type alias Model =
    { now : Posix
    , hours : String
    , minutes : String
    , seconds : String
    , id : Int
    }


init : ( Model, Cmd Msg )
init =
    ( Model (Time.millisToPosix 0) "" "" "" 0, Task.perform Now Time.now )



-- UPDATE


type Msg
    = Now Posix
    | Hours String
    | Minutes String
    | Seconds String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Now now ->
            ( { model | now = now }, Cmd.none )

        Hours hours ->
            ( { model | hours = hours }, Cmd.none )

        Minutes minutes ->
            ( { model | minutes = minutes }, Cmd.none )

        Seconds seconds ->
            ( { model | seconds = seconds }, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    div
        [ style "background" "white"
        , style "padding-top" "6rem"
        , style "height" "100vh"
        , style "weight" "100vw"
        ]
        [ title
        , viewForm model
        , p
            [ style "text-align" "center" ]
            [ text <| Iso8601.fromTime (calcDeadline (posixMidnight model.now) model.hours model.minutes model.seconds) ]
        ]


title =
    h1
        [ style "color" "rgba(0, 0, 0, 0.8)"
        , style "font-size" "2em"
        , style "text-align" "center"
        , style "padding-bottom" "5rem"
        , style "letter-spacing" "0.2rem"
        ]
        [ text "Countdowner" ]


viewForm : Model -> Html Msg
viewForm model =
    div
        [ style "display" "flex"
        , style "align-items" "center"
        , style "justify-content" "center"
        ]
        [ viewInput "number" "Hours" model.hours Hours
        , viewInput "number" "Minutes" model.minutes Minutes
        , viewInput "number" "Seconds" model.seconds Seconds
        ]


viewInput : String -> String -> String -> (String -> msg) -> Html msg
viewInput t p v toMsg =
    input
        [ type_ t
        , placeholder p
        , value v
        , onInput toMsg
        , style "padding" "10px 20px"
        , style "margin-right" "10px"
        ]
        []
