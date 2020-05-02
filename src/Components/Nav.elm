module Components.Nav exposing (view)

import Context
import Cx exposing (att, c)
import Html exposing (Html, a, button, div, i, img, nav, span, text)
import Html.Attributes exposing (href, src)
import Html.Events exposing (onClick)
import Layout.Routes exposing (rhref)
import Ports.Auth exposing (UserInfo)


view : String -> Maybe String -> Maybe UserInfo -> Html msg
view title maybeBackTo maybeCred =
    nav [ c "nav" ]
        [ div [ c "nav-control" ]
            [ case maybeBackTo of
                Just backTo ->
                    a [ href backTo, c "nav-back-button" ]
                        [ i [ c "icon-arrow-back" ] []
                        ]

                Nothing ->
                    text ""
            , div [ c "nav-page-title" ]
                [ if title == "" then
                    a [ href "/" ] [ img [ c "nav-page-title-logo", src "logo.png" ] [] ]

                  else
                    text title
                ]
            , case maybeCred of
                Just cred ->
                    div [ c "nav-userinfo" ]
                        [ text cred.email
                        , text " | "
                        , a [ rhref Layout.Routes.SignOut ] [ text "Salir" ]
                        ]

                Nothing ->
                    div [ c "nav-userinfo" ]
                        [ a [ rhref Layout.Routes.Identify ] [ text "Identificarse" ]
                        ]
            ]
        ]
