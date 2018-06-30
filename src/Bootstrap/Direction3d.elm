module Bootstrap.Direction3d exposing (components, unsafe)

import Geometry.Types exposing (..)


unsafe : ( Float, Float, Float ) -> Direction3d
unsafe =
    Direction3d


components : Direction3d -> ( Float, Float, Float )
components (Direction3d components_) =
    components_
