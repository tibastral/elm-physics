module StyleModifiers exposing (..)

import Elegant exposing (Vector, Style, toPx)


absolutelyPositionned : Vector -> Style -> Style
absolutelyPositionned ( x, y ) a =
    { a
        | position = Just "absolute"
        , left = Just (x |> toPx)
        , top = Just (y |> toPx)
        , right = Nothing
        , bottom = Nothing
    }
