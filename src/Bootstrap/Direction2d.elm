module Bootstrap.Direction2d
    exposing
        ( components
        , perpendicularTo
        , reverse
        , unsafe
        )

import Geometry.Types exposing (..)


unsafe : ( Float, Float ) -> Direction2d
unsafe =
    Direction2d


components : Direction2d -> ( Float, Float )
components (Direction2d components_) =
    components_


reverse : Direction2d -> Direction2d
reverse direction =
    let
        ( x, y ) =
            components direction
    in
    unsafe ( -x, -y )


perpendicularTo : Direction2d -> Direction2d
perpendicularTo direction =
    let
        ( x, y ) =
            components direction
    in
    unsafe ( -y, x )
