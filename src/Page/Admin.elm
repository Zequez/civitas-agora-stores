module Page.Admin exposing (Model, Msg, init, toContext, update, view)

import Api exposing (WebData(..))
import Components.Nav as Nav
import Context
import Cx exposing (c)
import Html exposing (Html, button, div, text)
import Html.Attributes
import Html.Events exposing (onClick)
import Layout.Routes
import Ports.Auth exposing (UserInfo)
import Types.Store exposing (Store, storesDecoder)



-- TYPES --


type alias Model =
    { context : Context.Model
    , stores : WebData (List Store)
    , edit : Maybe Int
    }



-- INIT --


config : { backTo : Maybe String, auth : Bool }
config =
    { backTo = Just "/", auth = True }


init : Context.Model -> ( Model, Cmd Msg )
init context =
    { context = context
    , stores = NotAsked
    , edit = Nothing
    }
        |> update RequestStores


ifLoggedIn : Context.Model -> a -> a -> a
ifLoggedIn context yes no =
    if context.cred == Nothing then
        no

    else
        yes


toContext : Model -> Context.Model
toContext { context } =
    context



-- Nothing ->
-- ( defaults, Layout.Routes.identifyAndRedirect contextModel.navKey Layout.Routes.Admin )
-- UPDATE --


type Msg
    = RequestStores
    | ReceiveStores (WebData (List Store))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        RequestStores ->
            if model.context.cred /= Nothing then
                ( { model | stores = Loading }, Api.stores ReceiveStores storesDecoder )

            else
                ( model, Cmd.none )

        ReceiveStores stores ->
            ( { model | stores = stores }, Cmd.none )



-- _ ->
--     ( model, Cmd.none )
-- VIEW --


view : Model -> { title : String, backTo : Maybe String, content : Html Msg }
view model =
    { title = "Administración"
    , backTo = Just "/"
    , content =
        if model.context.cred /= Nothing then
            div [ c "container" ]
                [ viewStoresList model
                ]

        else
            div [ c "container" ] [ text "Debe identificarse para continuar" ]
    }


viewStoresList : Model -> Html Msg
viewStoresList model =
    div [ c "admin-panel" ]
        [ case model.stores of
            NotAsked ->
                button [] [ text "Recargar" ]

            Loading ->
                text "Cargando"

            Failure err ->
                text (Api.errorToString err)

            Success stores ->
                div []
                    (stores
                        |> List.map (\s -> div [] [ text s.name ])
                    )
        ]
