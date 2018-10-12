module TimeFormatter exposing (calcDeadline, formatTime, posixMidnight)

import Time exposing (Posix)


secondConst =
    1000


minuteConst =
    secondConst * 60


hourConst =
    minuteConst * 60


dayConst =
    hourConst * 24


posixMidnight : Posix -> Int
posixMidnight posixTime =
    (Time.posixToMillis posixTime // dayConst) * dayConst


calcDeadline : Int -> String -> String -> String -> Posix
calcDeadline time hours minutes seconds =
    let
        hoursMillies =
            Maybe.withDefault 0 (String.toInt hours) * hourConst

        minutesMillies =
            Maybe.withDefault 0 (String.toInt minutes) * minuteConst

        secondsMillies =
            Maybe.withDefault 0 (String.toInt seconds) * secondConst
    in
    Time.millisToPosix (time + hoursMillies + minutesMillies + secondsMillies)


formatTime : Int -> String
formatTime millies =
    let
        hours =
            millies // hourConst

        minutes =
            remainderBy hourConst millies // minuteConst

        secondes =
            remainderBy minuteConst millies // secondConst
    in
    String.fromInt hours ++ "h " ++ getTimeString minutes ++ "m " ++ getTimeString secondes ++ "s"


getTimeString time =
    String.padLeft 2 '0' <| String.fromInt time
