module Elegant
    exposing
        ( Vector
        , Style
        , absolutelyPositionned
        , style
        )

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


toPx : Int -> String
toPx val =
    (val |> toString) ++ "px"


getStyles : Style -> List ( String, Maybe String )
getStyles (Style styleValues) =
    List.map
        (\( attrName, fun ) -> ( attrName, fun styleValues ))
        [ ( "position", .position )
        , ( "left", .left )
        , ( "top", .top )
        , ( "bottom", .bottom )
        , ( "right", .right )
        ]


compose : List (a -> a) -> (a -> a)
compose =
    List.foldr (<<) identity


toInlineStylesApply : Style -> (Style -> List ( String, Maybe String )) -> List ( String, String )
toInlineStylesApply result styles =
    styles result
        |> List.concatMap
            (\( attr, val ) ->
                case val of
                    Nothing ->
                        []

                    Just val_ ->
                        [ ( attr, val_ ) ]
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
            , left = Just (x |> round |> toPx)
            , top = Just (y |> round |> toPx)
            , right = Nothing
            , bottom = Nothing
        }
