module Main exposing (main)

-- import Layout.Layout as Layout

import Browser
import Context
import Json.Decode as D
import Layout.Wiring as Layout
import Ports.Auth
import Ports.Nav



-- import Browser exposing (Document)
---- PROGRAM ----


subscriptions : Layout.Model -> Sub Layout.Msg
subscriptions model =
    Sub.batch
        [-- Ports.Nav.navOn Layout.NavOn
         --   Ports.Auth.gotUserInfo
         --     (D.decodeValue Ports.Auth.userInfoDecoder
         --         >> Context.GotUserInfo
         --         >> Layout.ContextMsg
         --     )
        ]


main : Program () Layout.Model Layout.Msg
main =
    Browser.application
        { view = Layout.view
        , init = \_ url navKey -> Layout.init url navKey
        , update = Layout.update
        , subscriptions = subscriptions
        , onUrlChange = Layout.OnUrlChange
        , onUrlRequest = Layout.OnUrlRequest
        }
