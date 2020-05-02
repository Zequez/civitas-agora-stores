module Components.Switch exposing (view)

import Html exposing (Html, input, label, span)
import Html.Attributes exposing (checked, class, id, type_)
import Html.Events exposing (onCheck)


view : Bool -> (Bool -> msg) -> String -> Html msg
view checked_ checkMsg id_ =
    label [ class "switch" ]
        [ input
            [ id id_
            , type_ "checkbox"
            , checked checked_
            , onCheck checkMsg
            ]
            []
        , span [ class "slider" ] []
        ]
