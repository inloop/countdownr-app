module Api exposing (deadlineUrl)

import Url.Builder as Url


api : String
api =
    "https://countdownr.herokuapp.com"


deadlineUrl : String
deadlineUrl =
    Url.crossOrigin api [ "graphql" ] []
