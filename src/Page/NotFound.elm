module Page.NotFound exposing (view)

import Cx exposing (c)
import Html exposing (Html, div, h1, h2, main_, p, section, text)


view : { title : String, backTo : Maybe String, content : Html msg }
view =
    { title = "No encontrado"
    , backTo = Nothing
    , content = div [] [ text "Página no encontrada" ]
    }
