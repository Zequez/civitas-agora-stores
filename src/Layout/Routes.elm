module Layout.Routes exposing
    (  Route(..)
       -- , identifyAndRedirect

    , rhref
    , routeToString
    , urlToRoute
    )

import Browser.Navigation as Nav
import Html as H
import Html.Attributes as At
import Url exposing (Url)
import Url.Parser as Parser exposing ((</>), (<?>), Parser, oneOf, s, string)
import Url.Parser.Query as Query


type Route
    = Home
      -- | Store String
    | Identify
    | Admin
    | SignOut


parser : Parser (Route -> a) a
parser =
    oneOf
        [ Parser.map Home Parser.top
        , Parser.map Identify (s "identificarse")
        , Parser.map Admin (s "admin")
        , Parser.map SignOut (s "salir")
        ]


urlToRoute : Url -> Maybe Route
urlToRoute url =
    Parser.parse parser url



-- pathToRoute : String -> Route
-- pathToRoute path =
--     if path == "/" then
--         Home
--     else if path == "/admin" then
--         Admin
--         -- else if path == "/s" then
--         --     Store "tienda"
--     else if path == "/identificarse" then
--         Identify


routeToString : Route -> String
routeToString route =
    "/" ++ String.join "/" (routeToFragments route)


routeToFragments : Route -> List String
routeToFragments route =
    case route of
        Home ->
            []

        -- Store storeName ->
        --     "/s/" ++ storeName
        Identify ->
            [ "identificarse" ]

        Admin ->
            [ "admin" ]

        SignOut ->
            [ "salir" ]



-- identifyAndRedirect : Nav.Key -> Route -> Cmd msg
-- identifyAndRedirect key redirectTo =
--     Nav.replaceUrl key (routeToString (Identify (Just (routeToString redirectTo))))


rhref : Route -> H.Attribute msg
rhref route =
    At.href (routeToString route)
