module Page.Storefront exposing (Model, Msg, init, update, view)

import Contact exposing (whatsappLinkGen)
import Cx exposing (att, c, svgC)
import Data.Store as Store
import Html exposing (Html, a, article, button, div, figure, h1, h2, h3, i, img, li, main_, nav, p, section, span, text, ul)
import Html.Attributes exposing (href, src, target)
import Markdown.Parser
import Page.NotFound
import SharedState exposing (SharedState)
import Tag.Distribution exposing (Distribution)
import Tag.Modality exposing (Modality)
import Tag.Product exposing (Product)
import Tag.Ready exposing (Ready)
import Tag.SaleChannel exposing (SaleChannel)


init : SharedState -> String -> ( Model, Cmd Msg )
init session storeSlug =
    ( { session = session
      , store = Store.fromSlug storeSlug
      }
    , Cmd.none
    )


type alias Model =
    { session : SharedState
    , store : Maybe Store.Store
    }


type Msg
    = Nope


type alias Listing =
    { name : String
    , products : List Product
    }


type alias Product =
    { name : String
    , price : Float
    , description : String
    , thumbnail : String
    , stock : Int
    }


dummyListings : List Listing
dummyListings =
    [ Listing "Lorem ipsum"
        [ Product "Dolor sit amet" 80.0 "" "" 10
        , Product "Maecenas ut nunc" 75.0 "" "" -1
        , Product "Nunc iaculis erat " -1 "" "" 10
        , Product "Sed et ipsum a ipsum" 120.5 "" "" 10
        , Product "Proin hendrerit arcu" 30.0 "" "" 10
        ]
    , Listing "Consectetur"
        [ Product "Donec at eros" 60.4 "" "" 5
        , Product "Quisque mattis" 50 "" "" 10
        , Product "Ut in tortor" 70 "" "" 1
        , Product "Nam eget" 20 "" "" 5
        , Product "Nullam nec tortor" 30 "" "" -1
        , Product "Phasellus efficitur" 200 "" "" 5
        ]
    ]



-------------------------------- UPDATES


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )



-------------------------------- VIEWS


view : Model -> { title : String, content : Html Msg }
view { store } =
    case store of
        Nothing ->
            Page.NotFound.view

        Just sureStore ->
            trueView sureStore


trueView : Store.Store -> { title : String, content : Html Msg }
trueView store =
    { title = store.name
    , content =
        main_ []
            [ section [ c "hero is-light has-text-centered" ]
                [ div [ c "hero-body" ]
                    [ div [ c "container" ]
                        [ img [ src store.logo ] []
                        , h1 [ c "title" ] [ text store.name ]
                        ]
                    ]
                ]
            , section [ c "section" ]
                [ div [ c "responsive-container" ]
                    [ div [ c "store-content box" ]
                        [ tagsListView
                            "Modalidad de venta"
                            "is-light is-info "
                            Tag.Modality.toString
                            store.modality
                        , tagsListView
                            "Distribución"
                            "is-light is-info "
                            Tag.Distribution.toString
                            store.distribution
                        , tagsListView
                            "Canales de venta"
                            "is-light is-info "
                            Tag.SaleChannel.toString
                            store.saleChannel
                        , tagsListView
                            "Preparado en"
                            "is-light is-info "
                            Tag.Ready.toString
                            store.ready
                        , tagsListView
                            "Tipo de productos"
                            "is-light is-primary "
                            Tag.Product.toString
                            store.productsTags
                        ]
                    , article [ c "message store-content" ]
                        [ div
                            [ c "message-body content" ]
                            (case markdownView store.description of
                                Ok rendered ->
                                    rendered

                                Err errors ->
                                    [ text errors ]
                            )
                        ]
                    , div [ c "store-content store-listings" ]
                        (dummyListings
                            |> List.map listingView
                        )
                    ]
                ]
            , a
                [ c "button floating-call is-large"
                , href (whatsappLinkGen store.phone)
                , att "aria-label" "Llamar"
                , target "_blank"
                ]
                [ span [ c "icon is-medium" ]
                    [ i [ svgC "fab fa-2x fa-whatsapp" ] []
                    ]
                ]
            ]
    }


tagsListView : String -> String -> (a -> String) -> List a -> Html msg
tagsListView title classString toStringFun tagsList =
    div [ c "columns is-mobile" ]
        [ div [ c "column is-6 has-text-right" ] [ text title ]
        , div [ c "column is-6 tags" ]
            (tagsList
                |> List.map (\tag -> span [ c ("tag " ++ classString) ] [ text (toStringFun tag) ])
            )
        ]


markdownView : String -> Result String (List (Html msg))
markdownView markdown =
    markdown
        |> Markdown.Parser.parse
        |> Result.mapError (\error -> error |> List.map Markdown.Parser.deadEndToString |> String.join "\n")
        |> Result.andThen (Markdown.Parser.render Markdown.Parser.defaultHtmlRenderer)


listingView : Listing -> Html msg
listingView listing =
    div [ c "products-listing" ]
        [ h2 [ c "is-size-3" ] [ text listing.name ]
        , div [ c "products-grid" ]
            (listing.products
                |> List.map productTileView
            )
        ]


productTileView : Product -> Html msg
productTileView product =
    div [ c "products-grid-item" ]
        [ img [ src "/256x256.png" ] []
        , div [ c "products-grid-item-info" ]
            [ div [ c "products-grid-item-price is-size-4" ] [ text ("$" ++ String.fromFloat product.price) ]
            , div [ c "products-grid-item-name" ] [ text product.name ]
            ]
        ]
