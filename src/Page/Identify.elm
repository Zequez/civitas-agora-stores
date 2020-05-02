module Page.Identify exposing (Model, Msg, init, toContext, update, view)

import Context
import Cx exposing (c)
import Html exposing (Html, button, div, input, p, text)
import Html.Attributes exposing (placeholder, type_, value)
import Html.Events exposing (onClick, onInput)


type alias Model =
    { context : Context.Model
    , email : String
    , password : String
    }


type Msg
    = SignIn
    | SetEmail String
    | SetPassword String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetEmail email ->
            ( { model | email = email }, Cmd.none )

        SetPassword password ->
            ( { model | password = password }, Cmd.none )

        SignIn ->
            ( model, Cmd.none )


init : Context.Model -> ( Model, Cmd Msg )
init context =
    ( { context = context
      , email = ""
      , password = ""
      }
    , Cmd.none
    )


toContext : Model -> Context.Model
toContext { context } =
    context



-- init : Context.Model -> Maybe String -> ( Model, Cmd Msg )
-- init contextModel maybeRedirect =
--     ()


view : Model -> { title : String, backTo : Maybe String, content : Html Msg }
view model =
    { title = "Identificarse"
    , backTo = Just "/"
    , content =
        div [ c "container" ]
            [ div [ c "identify" ]
                [ div [ c "card" ]
                    [ p [] [ text "Ingrese sus datos" ]
                    , div [ c "inputs" ]
                        [ div []
                            [ input
                                [ c "input"
                                , value model.email
                                , onInput SetEmail
                                , placeholder "Email"
                                ]
                                []
                            ]
                        , div []
                            [ input
                                [ c "input"
                                , type_ "password"
                                , value model.password
                                , onInput SetPassword
                                , placeholder "Contraseña"
                                ]
                                []
                            ]
                        ]
                    , button [ c "btn", onClick SignIn ] [ text "Ingreso o registro" ]
                    ]
                ]
            ]
    }
