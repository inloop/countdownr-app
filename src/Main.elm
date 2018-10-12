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
import TimeFormatter exposing (formatTime)


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { left : Int
    , running : Bool
    , isDone : Bool
    , deadline : Posix
    , time : Posix
    , zone : Int -- timzone in minutes
    }


init : ( Model, Cmd Msg )
init =
    ( Model 0 False False (Time.millisToPosix 0) (Time.millisToPosix 0) 120
    , send (Fetch 39)
    )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    if model.running then
        Time.every 1000 Tick

    else
        Sub.none



-- UPDATE


send : msg -> Cmd msg
send msg =
    Task.succeed msg
        |> Task.perform identity


type Msg
    = Tick Posix
    | Start Posix
    | Fetch Int
    | Fetched (Result Http.Error String)
    | ToPosix (Result (List DeadEnd) Posix)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick time ->
            let
                timeZoneMillies =
                    model.zone * 60 * 1000

                deadlineMillies =
                    Time.posixToMillis model.deadline

                timeMillies =
                    Time.posixToMillis time

                left =
                    deadlineMillies - timeMillies - timeZoneMillies

                isDone =
                    left < 0
            in
            ( { model | left = left, isDone = isDone, running = not isDone }
            , Cmd.none
            )

        Start deadline ->
            ( { model | running = True }, Cmd.none )

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
                    ( { model | deadline = deadline }, send (Start model.deadline) )

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
        , counterView model

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


counterView model =
    p
        [ style "color" "rgba(255, 255, 255, 1)"
        , style "font-size" "8em"
        , style "font-weight" "bold"
        , style "text-align" "center"
        , style "text-shadow" "rgb(93, 253, 223) 3px 3px 0px, rgb(252, 0, 102) -3px -3px 0px"
        , style "letter-spacing" "3px"
        ]
        [ text <|
            if model.isDone then
                "Finished!"

            else
                formatTime model.left
        ]
