module Main exposing (main)

import Api exposing (deadlineUrl)
import Browser
import Debug
import Html exposing (..)
import Html.Attributes exposing (style)
import Http
import Iso8601
import Json.Decode as Decode
import Json.Encode as Encode
import Parser exposing (..)
import Task
import Time exposing (Posix)



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
    { deadline : Posix }


init : ( Model, Cmd Msg )
init =
    ( Model (Time.millisToPosix 0)
    , send (Fetch 39)
    )



-- UPDATE


send : msg -> Cmd msg
send msg =
    Task.succeed msg
        |> Task.perform identity


type Msg
    = Fetch Int
    | Fetched (Result Http.Error String)
    | ToPosix (Result (List DeadEnd) Posix)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Fetch id ->
            ( model, fetchDeadline model )

        Fetched result ->
            case result of
                Ok timeStamp ->
                    ( model, send (ToPosix (Iso8601.toTime timeStamp)) )

                Err _ ->
                    ( model, Cmd.none )

        ToPosix result ->
            case result of
                Ok deadline ->
                    ( { model | deadline = deadline }, Cmd.none )

                Err _ ->
                    ( model, Cmd.none )



-- HTTP


encodeBody : String -> Encode.Value
encodeBody value =
    Encode.object
        [ ( "query", Encode.string ("query { deadline(id:" ++ value ++ ") { date }}") ) ]


fetchDeadline : Model -> Cmd Msg
fetchDeadline model =
    Http.send Fetched (Http.post deadlineUrl (encodeBody "56" |> Http.jsonBody) responseDecoder)


responseDecoder : Decode.Decoder String
responseDecoder =
    Decode.field "data" (Decode.field "deadline" (Decode.field "date" Decode.string))



-- VIEW


view : Model -> Html Msg
view model =
    div
        [ style "background" "#010001"
        , style "padding-top" "6rem"
        , style "height" "100vh"
        , style "weight" "100vw"
        ]
        [ titleView
        , p
            [ style "color" "rgba(255, 255, 255, 0.8)" ]
            [ text <| Debug.toString <| model.deadline ]

        -- , p [ style "color" "rgba(255, 255, 255, 0.8)" ] [ text <| Debug.toString <| model ]
        ]


titleView =
    h1
        [ style "color" "rgba(255, 255, 255, 0.8)"
        , style "font-size" "2em"
        , style "text-align" "center"
        , style "padding-bottom" "5rem"
        , style "letter-spacing" "0.2rem"
        ]
        [ text "Countdowner" ]
