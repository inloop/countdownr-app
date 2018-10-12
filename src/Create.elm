module Main exposing (main)

import Api exposing (deadlineUrl)
import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Http
import Iso8601
import Json.Decode exposing (Decoder, field, int)
import Json.Encode as Encode
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
    , err : String
    , sent : Bool
    }


init : ( Model, Cmd Msg )
init =
    ( Model (Time.millisToPosix 0) "" "" "" 0 "" False, Task.perform Now Time.now )



-- UPDATE


type Msg
    = Now Posix
    | Hours String
    | Minutes String
    | Seconds String
    | Send Posix
    | Resolved (Result Http.Error Int)


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

        Resolved result ->
            case result of
                Ok id ->
                    ( { model | sent = True, id = id }, Cmd.none )

                Err err ->
                    ( { model | err = Debug.toString <| err }, Cmd.none )

        Send deadline ->
            ( model, createDeadline (Iso8601.fromTime deadline) )



-- HTTP


encodeBody : String -> Encode.Value
encodeBody value =
    Encode.object
        [ ( "query"
          , Encode.string "mutation($date: Date) { createDeadline(input:{ date:$date }) { id }}"
          )
        , ( "variables"
          , Encode.object
                [ ( "date"
                  , Encode.string value
                  )
                ]
          )
        ]


createDeadline value =
    Http.send Resolved (Http.post deadlineUrl (encodeBody value |> Http.jsonBody) responseDecoder)


responseDecoder : Decoder Int
responseDecoder =
    field "data" (field "createDeadline" (field "id" int))



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
        , if model.sent then
            viewSent model

          else
            viewForm model
        , p [] [ text <| model.err ]
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


viewSent model =
    div
        []
        [ p
            [ style "color" "black"
            , style "font-size" "8em"
            , style "font-weight" "bold"
            , style "text-align" "center"
            , style "letter-spacing" "3px"
            , style "padding-bottom" "2rem"
            , style "text-shadow" "rgb(93, 253, 223) 3px 3px 0px"
            ]
            [ text ("Created with id: " ++ String.fromInt model.id) ]
        , a
            [ href ("index.html?id=" ++ String.fromInt model.id)
            , style "border-bottom" "3px solid rgb(252, 0, 102)"
            , style "color" "rgb(252, 0, 102)"
            , style "text-decoration" "none"
            , style "font-size" "1.5em"
            , style "text-transform" "uppercase"
            , style "position" "absolute"
            , style "left" "50%"
            , style "transform" "translateX(-50%)"
            , style "padding" "7px 15px"
            ]
            [ text "Go to ->" ]
        ]



--


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
        , button
            [ onClick (Send (calcDeadline (posixMidnight model.now) model.hours model.minutes model.seconds))
            , style "background" "white"
            , style "border" "3px solid rgb(252, 0, 102)"
            , style "color" "rgb(252, 0, 102)"
            , style "font-weight" "bold"
            , style "padding" "8px 40px"
            , style "cursor" "pointer"
            ]
            [ text "CREATE" ]
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
