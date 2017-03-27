module Elegant exposing (style, Vector, Style, toPx, absolutelyPositionned)

import Html.Attributes
import Html exposing (Html)


type alias Vector =
    ( Float, Float )


type Style
    = Style
        { position : Maybe String
        , left : Maybe String
        , top : Maybe String
        , bottom : Maybe String
        , right : Maybe String
        }


type alias StyleTransformer =
    Style -> Style


toPx : a -> String
toPx val =
    (val |> toString) ++ "px"


getStyles : Style -> List ( Maybe String, String )
getStyles (Style styleValues) =
    List.map
        (\( fun, attrName ) -> ( fun styleValues, attrName ))
        [ ( .position, "position" )
        , ( .left, "left" )
        , ( .top, "top" )
        , ( .bottom, "bottom" )
        , ( .right, "right" )
        ]


compose : List (a -> a) -> (a -> a)
compose =
    List.foldr (<<) identity


toInlineStylesApply : Style -> (Style -> List ( Maybe String, String )) -> List ( String, String )
toInlineStylesApply result styles =
    styles result
        |> List.concatMap
            (\( maybe_, attr ) ->
                case maybe_ of
                    Nothing ->
                        []

                    Just val ->
                        [ ( attr, val ) ]
            )


toInlineStyles : Style -> StyleTransformer -> List ( String, String )
toInlineStyles default styleTransformer =
    toInlineStylesApply (styleTransformer default) getStyles


styleApply : List StyleTransformer -> List ( String, String )
styleApply styleTransformers =
    toInlineStyles
        (Style
            { position = Nothing
            , left = Nothing
            , top = Nothing
            , bottom = Nothing
            , right = Nothing
            }
        )
        (styleTransformers |> compose)


style : List StyleTransformer -> Html.Attribute msg
style styleTransformers =
    Html.Attributes.style (styleApply styleTransformers)


absolutelyPositionned : Vector -> Style -> Style
absolutelyPositionned ( x, y ) (Style style) =
    Style
        { style
            | position = Just "absolute"
            , left = Just (x |> toPx)
            , top = Just (y |> toPx)
            , right = Nothing
            , bottom = Nothing
        }
