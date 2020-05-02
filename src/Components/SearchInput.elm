module Components.SearchInput exposing (view)

import Cx exposing (att, c, cx)
import Html exposing (Html, a, div, i, input, nav, text)
import Html.Attributes exposing (autofocus, href, id, placeholder, value)
import Html.Events exposing (onClick, onInput)


view : String -> (String -> msg) -> String -> Html msg
view query inputMsg placeholdr =
    div [ c "search-input" ]
        [ i [ c "search-input-search icon-search" ] []
        , input
            [ value query
            , id "search-input"
            , onInput inputMsg
            , placeholder placeholdr
            , autofocus True
            ]
            []
        , i
            [ c "search-input-erase icon-close"
            , cx [ ( "search-input-erase-hidden", query == "" ) ]
            , onClick (inputMsg "")
            ]
            []
        ]



--   <div class="search-input">
--   <i class="search icon-search"></i>
--   <input
--     bind:value={query}
--     bind:this={searchInput}
-- 		on:keypress={(k) => k.key === "Enter" && done()}
--     placeholder="Buscar emprendimientos o productos..."/>
--   {#if query}
--     <i on:click={eraseQuery} class="erase icon-close"></i>
--   {/if}
-- </div>
