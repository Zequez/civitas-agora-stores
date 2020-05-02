module Page.Stores exposing
    ( Model
    , Msg
    , init
    , toContext
    , update
    , view
    )

import Browser.Dom
import Components.Nav as Nav
import Components.SearchInput as SearchInput
import Components.Switch as Switch
import Context
import Cx exposing (att, c, cx, svgC)
import Dict exposing (Dict)
import Html exposing (Html, a, button, div, i, img, input, label, li, main_, p, section, span, strong, text, ul)
import Html.Attributes as Attr exposing (for, href, src, style, target, title, type_, value)
import Html.Events exposing (onCheck, onClick, onInput)
import Json.Decode as Decode exposing (Decoder, bool, list, nullable, string)
import Json.Decode.Pipeline exposing (optional, required)
import List
import Ports.Auth
import String.Normalize
import Task



--                       ██╗███╗   ██╗██╗████████╗
--                       ██║████╗  ██║██║╚══██╔══╝
-- █████╗█████╗█████╗    ██║██╔██╗ ██║██║   ██║       █████╗█████╗█████╗
-- ╚════╝╚════╝╚════╝    ██║██║╚██╗██║██║   ██║       ╚════╝╚════╝╚════╝
--                       ██║██║ ╚████║██║   ██║
--                       ╚═╝╚═╝  ╚═══╝╚═╝   ╚═╝


init : Context.Model -> ( Model, Cmd Msg )
init context =
    ( { context = context
      , stores = Loading
      , storesResults = []
      , categories = Loading
      , categoriesById = Dict.fromList []
      , categoriesFlat = []
      , storesResultsPerCategory = Dict.fromList []
      , storeModal = Closed

      --
      , queryDelivery = False
      , querySearch = ""
      , queryCategoryId = ""
      }
    , Cmd.none
    )


toContext : Model -> Context.Model
toContext { context } =
    context



--                       ████████╗██╗   ██╗██████╗ ███████╗███████╗
--                       ╚══██╔══╝╚██╗ ██╔╝██╔══██╗██╔════╝██╔════╝
-- █████╗█████╗█████╗       ██║    ╚████╔╝ ██████╔╝█████╗  ███████╗    █████╗█████╗█████╗
-- ╚════╝╚════╝╚════╝       ██║     ╚██╔╝  ██╔═══╝ ██╔══╝  ╚════██║    ╚════╝╚════╝╚════╝
--                          ██║      ██║   ██║     ███████╗███████║
--                          ╚═╝      ╚═╝   ╚═╝     ╚══════╝╚══════╝


type alias Model =
    { context : Context.Model
    , stores : LoadStatus (List Store)
    , storesResults : List Store
    , categories : LoadStatus (List Category)
    , categoriesById : Dict String FlatCategory
    , categoriesFlat : List FlatCategory
    , storesResultsPerCategory : Dict String Int

    -- Obsolete
    , storeModal : StoreModalState

    ---- Query
    , queryDelivery : Bool
    , querySearch : String
    , queryCategoryId : String
    }


type LoadStatus data
    = Loading
    | Error String
    | Success data


type StoreModalState
    = Closed
    | Open Store


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


type alias Category =
    { id : String
    , name : String
    , subcat : SubCategories
    }


type alias FlatCategory =
    { id : String
    , name : String
    , subcat : List String
    , parent : Maybe String
    }


type SubCategories
    = SubCategories (List Category)


unwrapSubCategories : SubCategories -> List Category
unwrapSubCategories (SubCategories s) =
    s


emptyCategory : FlatCategory
emptyCategory =
    FlatCategory "" "" [] Nothing


getCategory : String -> Dict String FlatCategory -> FlatCategory
getCategory categoryId categoriesById =
    Maybe.withDefault emptyCategory (Dict.get categoryId categoriesById)


storesDecoder : Decoder (List Store)
storesDecoder =
    list storeDecoder


storeDecoder : Decoder Store
storeDecoder =
    Decode.succeed Store
        |> required "id" string
        |> required "name" string
        |> required "description" string
        |> required "categories" (list string)
        |> required "logo" (nullable string)
        |> required "address" (nullable string)
        |> required "delivery" bool
        |> required "deliveryInfo" (nullable string)
        |> required "productsSummary" string
        |> required "veganExclusive" bool
        |> required "isRestaurant" bool
        |> required "whatsapp" (nullable string)
        |> required "instagram" (nullable string)
        |> required "facebook" (nullable string)
        |> optional "textSearchCache" string ""


categoriesDecoder : Decoder (List Category)
categoriesDecoder =
    list categoryDecoder


categoryDecoder : Decoder Category
categoryDecoder =
    Decode.succeed Category
        |> required "id" string
        |> required "name" string
        |> optional "subcat" (Decode.lazy (\_ -> subCategoriesDecoder)) (SubCategories [])


subCategoriesDecoder : Decoder SubCategories
subCategoriesDecoder =
    Decode.map SubCategories <| Decode.list categoryDecoder


generateCategoriesById : List FlatCategory -> Dict String FlatCategory
generateCategoriesById categories =
    Dict.fromList (List.map (\c -> ( c.id, c )) categories)


generateFlatCategories : List Category -> List FlatCategory
generateFlatCategories categories =
    categories
        |> List.foldl
            (\cat acc ->
                generateFlatCategory Nothing cat
                    :: ((unwrapSubCategories cat.subcat
                            |> List.map (\c -> generateFlatCategory (Just cat) c)
                        )
                            ++ acc
                       )
            )
            []


generateFlatCategory : Maybe Category -> Category -> FlatCategory
generateFlatCategory parent category =
    { id = category.id
    , name = category.name
    , subcat = List.map .id (unwrapSubCategories category.subcat)
    , parent = Maybe.map .id parent
    }


type alias StoresQuery a =
    { a
        | queryDelivery : Bool
        , querySearch : String
        , queryCategoryId : String
        , categoriesById : Dict String FlatCategory
    }


filterStores : List Store -> StoresQuery a -> List Store
filterStores stores { queryDelivery, querySearch, queryCategoryId, categoriesById } =
    let
        categories : List String
        categories =
            if queryCategoryId == "" then
                []

            else
                queryCategoryId
                    :: Maybe.withDefault []
                        (Dict.get queryCategoryId categoriesById
                            |> Maybe.andThen (\s -> Just s.subcat)
                        )
    in
    stores
        |> pipeMatch queryDelivery (\store -> store.delivery)
        |> pipeMatch (not (List.isEmpty categories))
            (\store ->
                categories
                    |> List.any (\subcat -> List.member subcat store.categories)
            )
        |> pipeMatch (querySearch /= "")
            (\store ->
                String.contains querySearch store.textSearchCache
            )


fillStoresTextSearchCache : List Store -> List Store
fillStoresTextSearchCache stores =
    stores
        |> List.map
            (\store ->
                { store
                    | textSearchCache =
                        String.join " "
                            [ store.name
                            , store.description
                            , store.productsSummary
                            ]
                            |> normalizeString
                }
            )


normalizeString : String -> String
normalizeString str =
    str
        |> String.Normalize.removeDiacritics
        |> String.toLower


pipeMatch : Bool -> (Store -> Bool) -> (List Store -> List Store)
pipeMatch shouldMatch matcher =
    if shouldMatch then
        List.filter matcher

    else
        identity


countResultsPerCategory : List Store -> Dict String Int
countResultsPerCategory stores =
    stores
        |> List.map .categories
        |> List.concat
        |> List.foldl
            (\catId counters ->
                counters
                    |> Dict.update catId
                        (\maybeNum ->
                            case maybeNum of
                                Just num ->
                                    Just (num + 1)

                                Nothing ->
                                    Just 1
                        )
            )
            Dict.empty



-- , Cmd.batch
--     [ Api.stores GotStores storesDecoder
--     , Api.categories GotCategories categoriesDecoder
--     ]
-- )
--                       ██╗   ██╗██████╗ ██████╗  █████╗ ████████╗███████╗███████╗
--                       ██║   ██║██╔══██╗██╔══██╗██╔══██╗╚══██╔══╝██╔════╝██╔════╝
-- █████╗█████╗█████╗    ██║   ██║██████╔╝██║  ██║███████║   ██║   █████╗  ███████╗    █████╗█████╗█████╗
-- ╚════╝╚════╝╚════╝    ██║   ██║██╔═══╝ ██║  ██║██╔══██║   ██║   ██╔══╝  ╚════██║    ╚════╝╚════╝╚════╝
--                       ╚██████╔╝██║     ██████╔╝██║  ██║   ██║   ███████╗███████║
--                        ╚═════╝ ╚═╝     ╚═════╝ ╚═╝  ╚═╝   ╚═╝   ╚══════╝╚══════╝


type Msg
    = GotStores (LoadMsg (List Store))
    | GotCategories (LoadMsg (List Category))
    | ClickedDeliverySwitch Bool
    | WriteSearch String
    | CategorySelectChanged String
    | NoOp


type alias LoadMsg data =
    Result String data


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotStores result ->
            case result of
                Ok stores ->
                    ( updateSearchResults
                        { model | stores = Success (fillStoresTextSearchCache stores) }
                    , Cmd.none
                    )

                Err err ->
                    ( { model | stores = Error err }, Cmd.none )

        GotCategories result ->
            case result of
                Ok categories ->
                    let
                        flatCategories =
                            generateFlatCategories categories
                    in
                    ( { model
                        | categories = Success categories
                        , categoriesById = generateCategoriesById flatCategories
                        , categoriesFlat = flatCategories
                      }
                    , Cmd.none
                    )

                Err err ->
                    ( { model | categories = Error err }, Cmd.none )

        ClickedDeliverySwitch checked ->
            { model | queryDelivery = checked }
                |> updateSearchResults
                |> andCmdNone

        CategorySelectChanged categoryId ->
            { model | queryCategoryId = categoryId }
                |> updateSearchResults
                |> andCmdNone

        WriteSearch value ->
            { model | querySearch = value }
                |> updateSearchResults
                |> andCmd
                    (if value == "" then
                        Task.attempt (\_ -> NoOp) (Browser.Dom.focus "search-input")

                     else
                        Cmd.none
                    )

        NoOp ->
            ( model, Cmd.none )


updateSearchResults : Model -> Model
updateSearchResults model =
    case ( model.stores, model.categories ) of
        ( Success stores, Success categories ) ->
            let
                storesResults =
                    filterStores stores model
            in
            { model
                | storesResults = storesResults
                , storesResultsPerCategory = countResultsPerCategory storesResults
            }

        ( Success stores, _ ) ->
            { model | storesResults = filterStores stores model }

        ( _, _ ) ->
            model


andCmdNone : Model -> ( Model, Cmd Msg )
andCmdNone model =
    ( model, Cmd.none )


andCmd : Cmd Msg -> Model -> ( Model, Cmd Msg )
andCmd cmd model =
    ( model, cmd )



--                       ██╗   ██╗██╗███████╗██╗    ██╗
--                       ██║   ██║██║██╔════╝██║    ██║
-- █████╗█████╗█████╗    ██║   ██║██║█████╗  ██║ █╗ ██║    █████╗█████╗█████╗
-- ╚════╝╚════╝╚════╝    ╚██╗ ██╔╝██║██╔══╝  ██║███╗██║    ╚════╝╚════╝╚════╝
--                        ╚████╔╝ ██║███████╗╚███╔███╔╝
--                         ╚═══╝  ╚═╝╚══════╝ ╚══╝╚══╝


view : Model -> { title : String, backTo : Maybe String, content : Html Msg }
view model =
    { title = ""
    , backTo = Nothing
    , content =
        div [ c "stores-listing", cx [ ( "is-clipped", model.storeModal /= Closed ) ] ]
            [ -- section [ c "stores-header" ]
              -- [ a [ c "stores-header-back", href "https://mardel.org" ]
              --     [ i [ c "icon-apps" ] []
              --     ]
              -- , div [ c "stores-header-title" ]
              --     [ i [ c "icon-seedling" ] []
              --     , i [ c "icon-store" ] []
              --     , i [ c "icon-globe-americas" ] []
              --     ]
              -- ]
              section [ c "stores-filter" ]
                [ div [ c "stores-filter-container" ]
                    [ SearchInput.view model.querySearch WriteSearch "Buscar..."
                    , div [ c "stores-filter-extra" ]
                        [ div [ c "stores-filter-delivery" ]
                            [ Switch.view model.queryDelivery ClickedDeliverySwitch "delivery-switch"
                            , label [ for "delivery-switch" ] [ text "Delívery a domicilio" ]
                            ]
                        , viewCategoriesSelect model
                        ]
                    ]
                ]
            , if model.queryCategoryId /= "" || List.length model.storesResults <= 3 then
                section [ c "stores-list container" ]
                    [ div [ c "responsive-container" ]
                        (case model.stores of
                            Loading ->
                                [ text "Cargando" ]

                            Error err ->
                                [ text ("Error: " ++ err) ]

                            Success _ ->
                                model.storesResults
                                    |> List.map viewStore
                        )
                    ]

              else
                div [ c "container" ]
                    [ div [ c "categories-list" ]
                        ([ ( "comida", "categories/food-resized.jpg" )
                         , ( "dulce", "categories/reposteria.jpg" )
                         , ( "cosmetica", "categories/reposteria.jpg" )
                         , ( "viandas", "categories/reposteria.jpg" )
                         , ( "frutihorticola", "categories/reposteria.jpg" )
                         , ( "catering", "categories/reposteria.jpg" )
                         , ( "despensa", "categories/reposteria.jpg" )
                         , ( "restaurantes", "categories/reposteria.jpg" )
                         , ( "otros", "categories/reposteria.jpg" )
                         ]
                            |> List.map
                                (\( categoryId, imageSrc ) ->
                                    viewCategoryCard
                                        model.categoriesById
                                        model.storesResultsPerCategory
                                        categoryId
                                        imageSrc
                                )
                            |> List.filterMap identity
                        )
                    ]
            ]
    }


viewCategoryCard : Dict String FlatCategory -> Dict String Int -> String -> String -> Maybe (Html Msg)
viewCategoryCard categoriesById resultsPerCategory categoryId imageSrc =
    let
        resultsInCategory =
            Maybe.withDefault 0 (Dict.get categoryId resultsPerCategory)
    in
    if resultsInCategory > 0 then
        Just
            (div [ c "card category-card", onClick (CategorySelectChanged categoryId) ]
                [ div [ c "category-card-background", style "background-image" ("url(" ++ imageSrc ++ ")") ] []
                , div [ c "category-card-title" ]
                    [ text
                        (Maybe.withDefault categoryId
                            (Dict.get categoryId categoriesById
                                |> Maybe.andThen (\c -> Just c.name)
                            )
                        )
                    , span [ c "category-card-stores-count" ]
                        [ text (String.fromInt resultsInCategory)
                        ]
                    ]
                ]
            )

    else
        Nothing


type alias CategoriesSelectConfig a =
    { a
        | categories : LoadStatus (List Category)
        , categoriesById : Dict String FlatCategory
        , categoriesFlat : List FlatCategory
        , queryCategoryId : String
    }


viewCategoriesSelect : CategoriesSelectConfig a -> Html Msg
viewCategoriesSelect { categories, categoriesById, categoriesFlat, queryCategoryId } =
    case categories of
        Loading ->
            text "..."

        Error err ->
            text ("Error: " ++ err)

        Success _ ->
            div [ c "categories-select" ]
                [ if queryCategoryId == "" then
                    Html.select [ onInput CategorySelectChanged ]
                        (Html.option [ value "" ] [ text "Todas las categorías" ]
                            :: (categoriesFlat
                                    |> List.map (viewCategoryOption queryCategoryId)
                               )
                        )

                  else
                    div [ c "selected-category", onClick (CategorySelectChanged "") ]
                        [ text (getCategory queryCategoryId categoriesById).name
                        , i [ c "icon-close" ] []
                        ]
                ]


viewCategoryOption : String -> FlatCategory -> Html Msg
viewCategoryOption categoryId category =
    Html.option
        [ value category.id
        , Attr.selected (category.id == categoryId)
        ]
        [ text
            (if category.parent == Nothing then
                category.name

             else
                "\u{00A0}\u{00A0}\u{00A0}\u{00A0}" ++ category.name
            )
        ]


viewStore : Store -> Html Msg
viewStore store =
    div [ c "card store-card" ]
        [ div [ c "card-block" ]
            [ case store.logo of
                Just logo ->
                    div [ c "store-card-pic" ]
                        [ img [ src logo ] [] ]

                Nothing ->
                    div [] []
            , div [ c "store-card-content" ]
                [ div [ c "store-card-title" ]
                    [ div [ c "font-subtitle1" ] [ text store.name ]

                    -- , div [ c "store-card-expand" ]
                    --     [ span [ c "i" ] [ i [ svgC "fas fa-chevron-down" ] [] ] ]
                    ]
                , div [ c "store-card-subtitle" ]
                    [ viewStoreTag "fas fa-map-marker-alt" store.address
                    , viewStoreTag "fas fa-truck"
                        (if store.delivery then
                            Just "Delívery"

                         else
                            Nothing
                        )
                    ]
                ]
            ]

        -- , div [ c "card-block" ]
        --     [ case store.deliveryInfo of
        --         Just deliveryInfo ->
        --             div [] [ text deliveryInfo ]
        --         Nothing ->
        --             div [] []
        --     , div [ c "" ] [ text store.productsSummary ]
        --     ]
        , div [ c "card-links" ]
            ([ viewStoreAction
                "fab fa-instagram"
                "https://instagram.com/"
                store.instagram
                Nothing
             , viewStoreAction
                "fab fa-facebook"
                "https://facebook.com/"
                store.facebook
                (Just "Facebook")
             , viewStoreAction
                "fab fa-whatsapp"
                "https://api.whatsapp.com/send?phone="
                store.whatsapp
                (Just "WhatsApp")
             ]
                |> List.filterMap identity
            )
        ]


viewStoreAction : String -> String -> Maybe String -> Maybe String -> Maybe (Html Msg)
viewStoreAction icon urlBase maybeAddr maybeTxt =
    maybeAddr
        |> Maybe.map
            (\addr ->
                a [ href (urlBase ++ addr), target "_blank" ]
                    [ i [ svgC ("i " ++ icon) ] []
                    , text (Maybe.withDefault addr maybeTxt)
                    ]
            )


viewStoreTag : String -> Maybe String -> Html Msg
viewStoreTag icon maybeTxt =
    case maybeTxt of
        Just txt ->
            span [ c "store-card-tag" ]
                [ i [ svgC ("i " ++ icon) ] []
                , span [] [ text txt ]
                ]

        Nothing ->
            span [] []
