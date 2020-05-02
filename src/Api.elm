module Api exposing
    ( WebData(..)
    , categories
    , errorToString
    , fromResult
    , store
    , stores
    , url
    )

import Http
import Json.Decode exposing (Decoder)


type WebData data
    = NotAsked
    | Loading
    | Failure Http.Error
    | Success data


fromResult : Result Http.Error data -> WebData data
fromResult result =
    case result of
        Ok content ->
            Success content

        Err err ->
            Failure err


apiBaseUrl : String
apiBaseUrl =
    "https://civitas-agora-stores.firebaseio.com"


url : String -> String
url path =
    apiBaseUrl ++ path


get : String -> (WebData data -> msg) -> Decoder data -> Cmd msg
get path msg decoder =
    Http.get
        { url = url path
        , expect = Http.expectJson (fromResult >> msg) decoder
        }


stores : (WebData data -> msg) -> Decoder data -> Cmd msg
stores msg decoder =
    get "/stores.json" msg decoder


categories : (WebData data -> msg) -> Decoder data -> Cmd msg
categories msg decoder =
    get "/categories.json" msg decoder


store : (WebData data -> msg) -> Decoder data -> Cmd msg
store msg decoder =
    get "/store" msg decoder


errorToString : Http.Error -> String
errorToString err =
    case err of
        Http.BadUrl str ->
            "Bad URL error: " ++ str

        Http.Timeout ->
            "Timeout error"

        Http.NetworkError ->
            "Network error"

        Http.BadStatus headerNum ->
            "Error " ++ String.fromInt headerNum

        Http.BadBody str ->
            "Parse error: " ++ str
