module Bootstrap.Frame2d
    exposing
        ( originPoint
        , xDirection
        , yDirection
        )

import Geometry.Types exposing (..)


originPoint : Frame2d -> Point2d
originPoint (Frame2d properties) =
    properties.originPoint


xDirection : Frame2d -> Direction2d
xDirection (Frame2d properties) =
    properties.xDirection


yDirection : Frame2d -> Direction2d
yDirection (Frame2d properties) =
    properties.yDirection
