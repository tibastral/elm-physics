module Elegant exposing (style, Vector, Style, toPx)

import Html.Attributes
import Html exposing (Html)


type alias Vector =
    ( Float, Float )


type alias StyleModifier =
    Style -> Style


type alias Style =
    { position : Maybe String
    , left : Maybe String
    , top : Maybe String
    , bottom : Maybe String
    , right : Maybe String
    }


toPx : a -> String
toPx val =
    (val |> toString) ++ "px"


allStyles : List ( Style -> Maybe String, String )
allStyles =
    [ ( .position, "position" )
    , ( .left, "left" )
    , ( .top, "top" )
    , ( .bottom, "bottom" )
    , ( .right, "right" )
    ]


toInlineStylesApply : a -> List ( a -> Maybe b, c ) -> List ( c, b )
toInlineStylesApply result styles =
    case styles of
        ( fun, attr ) :: tail ->
            case (fun result) of
                Nothing ->
                    toInlineStylesApply result tail

                Just val ->
                    ( attr, val ) :: (toInlineStylesApply result tail)

        [] ->
            []


toInlineStyles : Style -> StyleModifier -> List ( String, String )
toInlineStyles default styleModifier =
    toInlineStylesApply (styleModifier default) allStyles


compose : List (a -> a) -> (a -> a)
compose modifiers =
    modifiers
        |> List.foldr (<<) identity


styleApply : List StyleModifier -> List ( String, String )
styleApply styleModifiers =
    toInlineStyles
        { position = Nothing
        , left = Nothing
        , top = Nothing
        , bottom = Nothing
        , right = Nothing
        }
        (styleModifiers |> compose)


style : List (Style -> Style) -> Html.Attribute msg
style styleModifiers =
    Html.Attributes.style (styleApply styleModifiers)
