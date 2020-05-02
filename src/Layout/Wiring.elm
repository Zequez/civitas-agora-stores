module Layout.Wiring exposing (Model, Msg(..), init, update, view)

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
import Page.NotFound
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
-- type alias Model =
--     { route : Maybe R.Route
--     , context : Context.Model
--     -- Pages models
--     , admin : Page.Admin.Model
--     , stores : Page.Stores.Model
--     }


type Model
    = StoresModel Page.Stores.Model
    | AdminModel Page.Admin.Model
    | IdentifyModel Page.Identify.Model
    | EmptyModel Context.Model


modelToContext : Model -> Context.Model
modelToContext model =
    case model of
        StoresModel subModel ->
            Page.Stores.toContext subModel

        AdminModel subModel ->
            Page.Admin.toContext subModel

        IdentifyModel subModel ->
            Page.Identify.toContext subModel

        EmptyModel context ->
            context


updateContext : Model -> Context.Model -> Model
updateContext model context =
    case model of
        StoresModel subModel ->
            StoresModel { subModel | context = context }

        AdminModel subModel ->
            AdminModel { subModel | context = context }

        IdentifyModel subModel ->
            IdentifyModel subModel

        EmptyModel _ ->
            EmptyModel context


init : Url -> Nav.Key -> ( Model, Cmd Msg )
init url navKey =
    initRoute (R.urlToRoute url) (EmptyModel (Context.init True navKey))



-- url
-- initRoute
-- { route = R.urlToRoute url
-- , context = Context.init True navKey
-- --------------------------
-- -- Pages initial states --
-- --------------------------
-- , stores = Page.Stores.defaults
-- , admin = Page.Admin.defaults
-- }
-- setModelFragment : Model -> PageModel -> Model
-- setModelFragment model pageModel =
--     case pageModel of
--         ContextModel subModel ->
--             { model | context = subModel }
--         -------------------------
--         -- Pages state mapping --
--         -------------------------
--         StoresModel subModel ->
--             { model | stores = subModel }
--         AdminModel subModel ->
--             { model | admin = subModel }


type Msg
    = Noop
      -- | NavOn Nav.NavData
    | OnUrlChange Url
    | OnUrlRequest Browser.UrlRequest
      --------------------
      -- Pages messages --
      --------------------
    | StoresMsg Page.Stores.Msg
    | AdminMsg Page.Admin.Msg
    | IdentifyMsg Page.Identify.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        -- NavOn navData ->
        --     initRoute { model | route = R.pathToRoute navData.path }
        ( OnUrlChange url, _ ) ->
            initRoute (R.urlToRoute url) model

        ( OnUrlRequest urlRequest, _ ) ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl (modelToContext model).navKey (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )

        -- (ContextMsg subMsg, _) ->
        --     Context.update subMsg modelToContext subMsg
        --         |> wrapUpdateModel model ContextModel
        --         |> wrapUpdateCmd ContextMsg
        -------------------
        -- Pages updates --
        -------------------
        ( StoresMsg subMsg, StoresModel subModel ) ->
            Page.Stores.update subMsg subModel
                |> wrapUpdate StoresModel StoresMsg

        ( AdminMsg subMsg, AdminModel subModel ) ->
            Page.Admin.update subMsg subModel
                |> wrapUpdate AdminModel AdminMsg

        ( IdentifyMsg subMsg, IdentifyModel subModel ) ->
            Page.Identify.update subMsg subModel
                |> wrapUpdate IdentifyModel IdentifyMsg

        ( _, _ ) ->
            ( model, Cmd.none )


initRoute : Maybe R.Route -> Model -> ( Model, Cmd Msg )
initRoute route model =
    let
        context =
            modelToContext model
    in
    case route of
        Just R.Home ->
            Page.Stores.init context
                |> wrapUpdate StoresModel StoresMsg

        Just R.Admin ->
            Page.Admin.init context
                |> wrapUpdate AdminModel AdminMsg

        Just R.Identify ->
            Page.Identify.init context
                |> wrapUpdate IdentifyModel IdentifyMsg

        _ ->
            ( model, Cmd.none )


wrapUpdate : (model -> Model) -> (msg -> Msg) -> ( model, Cmd msg ) -> ( Model, Cmd Msg )
wrapUpdate modelWrapper msgWrapper ( subModel, subCmd ) =
    ( modelWrapper subModel, Cmd.map msgWrapper subCmd )



-- signInAndRedirect : R.Route -> Model -> ( Model, Cmd Msg )
-- signInAndRedirect maybeRoute model =
--     Context.update (Context.SignInAndRedirect (Just maybeRoute)) model.context
--         |> wrapUpdateModel model ContextModel
--         |> wrapUpdateCmd ContextMsg
-- wrapUpdateModel : Model -> (model -> Model) -> ( model, Cmd msg ) -> ( Model, Cmd msg )
-- wrapUpdateModel model wrapper ( subModel, cmd ) =
--     ( wrapper subModel, cmd )
-- wrapUpdateCmd : (msg -> Msg) -> ( model, Cmd msg ) -> ( model, Cmd Msg )
-- wrapUpdateCmd msgWrapper ( model, subCmd ) =
--     ( model, Cmd.map msgWrapper subCmd )


view : Model -> Document Msg
view model =
    let
        _ =
            Debug.log "Context model" model

        { title, content } =
            case model of
                StoresModel subModel ->
                    Page.Stores.view subModel
                        |> wrapPageViewMsg StoresMsg model

                AdminModel subModel ->
                    Page.Admin.view subModel
                        |> wrapPageViewMsg AdminMsg model

                IdentifyModel subModel ->
                    Page.Identify.view subModel
                        |> wrapPageViewMsg IdentifyMsg model

                EmptyModel subModel ->
                    Page.NotFound.view
    in
    { title = title
    , body = [ content ]
    }


type alias PageView msg =
    { title : String
    , content : H.Html msg
    , backTo : Maybe String
    }


wrapPageViewMsg : (msg -> Msg) -> Model -> PageView msg -> PageView Msg
wrapPageViewMsg wrapper model { title, content, backTo } =
    let
        context =
            modelToContext model
    in
    { title =
        if title == "" then
            "Emprendimientos de Mar del Plata"

        else
            title
    , backTo = backTo
    , content =
        H.main_ []
            [ NavC.view title backTo context.cred
            , H.map wrapper content
            ]
    }
