module Context exposing (Model, Msg(..), init, update)

import Browser.Navigation as Nav
import Json.Decode as D
import Layout.Routes exposing (Route, routeToString)
import Ports.Auth exposing (AuthError, UserInfo)



-- import Ports.Nav


type alias Model =
    { navKey : Nav.Key
    , cred : Maybe UserInfo
    , redirectAfterSignIn : Maybe Route
    , expectSignIn : Bool
    }


type Msg
    = DoSignIn
    | DoSignOut
    | SignInAndRedirect (Maybe Route)
    | GotUserInfo (Result D.Error UserInfo)
    | GotAuthError AuthError


init : Bool -> Nav.Key -> Model
init expectSignIn navKey =
    { navKey = navKey
    , cred = Nothing
    , redirectAfterSignIn = Nothing
    , expectSignIn = expectSignIn
    }



-- signInAndRedirect : Maybe Route -> Model -> ( Model, Cmd Msg )
-- signInAndRedirect route model =
--     ( { model | redirectAfterSignIn = route }
--     , Nav.replaceUrl model.navKey (routeToString Layout.Routes.Identify)
--     )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        DoSignIn ->
            ( model, Ports.Auth.doSignIn () )

        DoSignOut ->
            ( model, Ports.Auth.doSignOut () )

        SignInAndRedirect route ->
            ( { model | redirectAfterSignIn = route }
            , Nav.replaceUrl model.navKey (routeToString Layout.Routes.Identify)
            )

        GotUserInfo userInfoResult ->
            case userInfoResult of
                -- Sign in
                Ok userInfo ->
                    ( { model | cred = Just userInfo }
                    , case model.redirectAfterSignIn of
                        Just route ->
                            Nav.replaceUrl model.navKey (routeToString route)

                        Nothing ->
                            Cmd.none
                    )

                -- Sign out
                Err _ ->
                    ( { model | cred = Nothing }, Cmd.none )

        GotAuthError authError ->
            let
                _ =
                    Debug.log "Auth Error" authError
            in
            ( model, Cmd.none )
