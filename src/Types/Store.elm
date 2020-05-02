module Types.Store exposing (Store, storeDecoder, storesDecoder)

import Json.Decode as D exposing (bool, list, nullable, string)
import Json.Decode.Pipeline as DP


type alias Store =
    { id : String
    , name : String
    , description : String
    , categories : List String
    , logo : Maybe String
    , address : Maybe String
    , delivery : Bool
    , deliveryInfo : Maybe String
    , productsSummary : String
    , veganExclusive : Bool
    , isRestaurant : Bool
    , whatsapp : Maybe String
    , instagram : Maybe String
    , facebook : Maybe String
    , textSearchCache : String
    }


storeDecoder : D.Decoder Store
storeDecoder =
    D.succeed Store
        |> DP.required "id" string
        |> DP.required "name" string
        |> DP.required "description" string
        |> DP.required "categories" (list string)
        |> DP.required "logo" (nullable string)
        |> DP.required "address" (nullable string)
        |> DP.required "delivery" bool
        |> DP.required "deliveryInfo" (nullable string)
        |> DP.required "productsSummary" string
        |> DP.required "veganExclusive" bool
        |> DP.required "isRestaurant" bool
        |> DP.required "whatsapp" (nullable string)
        |> DP.required "instagram" (nullable string)
        |> DP.required "facebook" (nullable string)
        |> DP.optional "textSearchCache" string ""


storesDecoder : D.Decoder (List Store)
storesDecoder =
    list storeDecoder
