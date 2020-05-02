module Layout.Layout exposing (Model, Msg(..), init, update, view)

-- import Ports.Nav as Nav

import Browser exposing (Document)
import Browser.Navigation as Nav
import Components.Nav as NavC
import Context
import Html as H
import Json.Encode
import Layout.Routes as R
import Page.Admin
import Page.Identify
import Page.Stores
import Ports.Auth
import Url exposing (Url)



-- type alias Page model msg =
--     { model : PModel model
--     , view : PModel model -> PContent msg
--     , update : msg -> PModel model -> ( PModel model, Cmd msg )
--     , authRequired : Bool
--     , route : R.Route
--     }
-- type alias PContent msg =
--     { title : String
--     , content : H.Html msg
--     , backTo : Maybe String
--     }
-- type alias PModel model =
--     { model
--         | route : Maybe R.Route
--         , navKey : Nav.Key
--         , cred : Maybe Ports.Auth.UserInfo
--         , expectSignIn : Bool
--     }


type alias Model =
    { route : Maybe R.Route
    , context : Context.Model

    -- Pages models
    , admin : Page.Admin.Model
    , stores : Page.Stores.Model
    }


type PageModel
    = ContextModel Context.Model
    | StoresModel Page.Stores.Model
    | AdminModel Page.Admin.Model


setModelFragment : Model -> PageModel -> Model
setModelFragment model pageModel =
    case pageModel of
        ContextModel subModel ->
            { model | context = subModel }

        -------------------------
        -- Pages state mapping --
        -------------------------
        StoresModel subModel ->
            { model | stores = subModel }

        AdminModel subModel ->
            { model | admin = subModel }


type Msg
    = Noop
      -- | NavOn Nav.NavData
    | OnUrlChange Url
    | OnUrlRequest Browser.UrlRequest
    | ContextMsg Context.Msg
      --------------------
      -- Pages messages --
      --------------------
    | StoresMsg Page.Stores.Msg
    | AdminMsg Page.Admin.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Noop ->
            ( model, Cmd.none )

        -- NavOn navData ->
        --     initRoute { model | route = R.pathToRoute navData.path }
        OnUrlChange url ->
            initRoute { model | route = R.urlToRoute url }

        OnUrlRequest urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.context.navKey (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )

        ContextMsg subMsg ->
            Context.update subMsg model.context
                |> wrapUpdateModel model ContextModel
                |> wrapUpdateCmd ContextMsg

        -------------------
        -- Pages updates --
        -------------------
        StoresMsg subMsg ->
            Page.Stores.update subMsg model.stores
                |> wrapUpdateModel model StoresModel
                |> wrapUpdateCmd StoresMsg

        AdminMsg subMsg ->
            case model.context.cred of
                Just userInfo ->
                    Page.Admin.update userInfo subMsg model.admin
                        |> wrapUpdateModel model AdminModel
                        |> wrapUpdateCmd AdminMsg

                Nothing ->
                    ( model, Cmd.none )


init : Url -> Nav.Key -> ( Model, Cmd Msg )
init url navKey =
    initRoute
        { route = R.urlToRoute url
        , context = Context.init True navKey

        --------------------------
        -- Pages initial states --
        --------------------------
        , stores = Page.Stores.defaults
        , admin = Page.Admin.defaults
        }


initRoute : Model -> ( Model, Cmd Msg )
initRoute model =
    case ( model.route, model.context.cred ) of
        ( Just R.Home, _ ) ->
            Page.Stores.init
                |> wrapUpdateModel model StoresModel
                |> wrapUpdateCmd StoresMsg

        ( Just R.Admin, Just userInfo ) ->
            Page.Admin.init userInfo
                |> wrapUpdateModel model AdminModel
                |> wrapUpdateCmd AdminMsg

        ( Just R.Admin, Nothing ) ->
            signInAndRedirect R.Admin model

        _ ->
            ( model, Cmd.none )


signInAndRedirect : R.Route -> Model -> ( Model, Cmd Msg )
signInAndRedirect maybeRoute model =
    Context.update (Context.SignInAndRedirect (Just maybeRoute)) model.context
        |> wrapUpdateModel model ContextModel
        |> wrapUpdateCmd ContextMsg


wrapUpdateModel : Model -> (model -> PageModel) -> ( model, Cmd msg ) -> ( Model, Cmd msg )
wrapUpdateModel model pageModelWrapper ( subModel, cmd ) =
    ( setModelFragment model (pageModelWrapper subModel), cmd )


wrapUpdateCmd : (msg -> Msg) -> ( model, Cmd msg ) -> ( model, Cmd Msg )
wrapUpdateCmd msgWrapper ( model, subCmd ) =
    ( model, Cmd.map msgWrapper subCmd )


view : Model -> Document Msg
view model =
    let
        _ =
            Debug.log "Context model" model.context

        { title, content } =
            case model.route of
                Just route ->
                    case route of
                        R.Home ->
                            Page.Stores.view model.stores
                                |> wrapPageViewMsg StoresMsg model.context

                        R.Admin ->
                            Page.Admin.view model.admin
                                |> wrapPageViewMsg AdminMsg model.context

                        R.Identify ->
                            Page.Identify.view
                                |> wrapPageViewMsg ContextMsg model.context

                Nothing ->
                    { title = "No encontrado", backTo = Nothing, content = H.div [] [ H.text "Página no encontrada" ] }
    in
    { title = title
    , body = [ content ]
    }


type alias PageView msg =
    { title : String
    , content : H.Html msg
    , backTo : Maybe String
    }


wrapPageViewMsg : (msg -> Msg) -> Context.Model -> PageView msg -> PageView Msg
wrapPageViewMsg wrapper context { title, content, backTo } =
    { title =
        if title == "" then
            "Emprendimientos de Mar del Plata"

        else
            title
    , backTo = backTo
    , content =
        H.main_ []
            [ H.map ContextMsg (NavC.view title backTo context.cred)
            , H.map wrapper content
            ]
    }
