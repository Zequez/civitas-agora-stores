-- import Time exposing (toYear, utc, millisToPosix)
-- townZone


module Extra.Time exposing (toText)

import Time exposing (Month(..), Posix, Weekday(..), Zone)


townTimeZone : Zone
townTimeZone =
    Time.customZone (-3 * 60) []


toText : Posix -> String
toText time =
    String.join " "
        [ toEsWeekday (Time.toWeekday townTimeZone time)
        , String.fromInt (Time.toDay townTimeZone time)
        , toEsMonth (Time.toMonth townTimeZone time)
        , String.fromInt (Time.toYear townTimeZone time)
        ]


toEsMonth : Month -> String
toEsMonth month =
    case month of
        Jan ->
            "Ene"

        Feb ->
            "Feb"

        Mar ->
            "Mar"

        Apr ->
            "Abr"

        May ->
            "May"

        Jun ->
            "Jun"

        Jul ->
            "Jul"

        Aug ->
            "Ago"

        Sep ->
            "Sep"

        Oct ->
            "Oct"

        Nov ->
            "Nov"

        Dec ->
            "Dic"


toEsWeekday : Weekday -> String
toEsWeekday weekday =
    case weekday of
        Mon ->
            "Lunes"

        Tue ->
            "Martes"

        Wed ->
            "Miércoles"

        Thu ->
            "Jueves"

        Fri ->
            "Viernes"

        Sat ->
            "Sábado"

        Sun ->
            "Domingo"
