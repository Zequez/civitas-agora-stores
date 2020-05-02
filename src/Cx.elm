module Cx exposing (att, c, cx, svgC)

import Html exposing (Attribute)
import Html.Attributes exposing (attribute, class, classList)
import Svg.Attributes


c : String -> Attribute msg
c =
    class



-- Font Awesome replaces the "<i>" element with an <svg> element
-- and then stuff breaks. https://github.com/elm/svg/issues/3


svgC : String -> Attribute msg
svgC =
    Svg.Attributes.class


cx : List ( String, Bool ) -> Attribute msg
cx =
    classList


att : String -> String -> Attribute msg
att =
    attribute
