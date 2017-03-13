module Tuple.Extra exposing (..)


map2 : (a -> a -> a) -> ( a, a ) -> ( a, a ) -> ( a, a )
map2 operator ( x1, y1 ) ( x2, y2 ) =
    ( operator x1 x2, operator y1 y2 )


add =
    map2 (+)
