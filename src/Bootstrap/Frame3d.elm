module Bootstrap.Frame3d
    exposing
        ( originPoint
        , xDirection
        , yDirection
        , zDirection
        )

import Geometry.Types exposing (..)


originPoint : Frame3d -> Point3d
originPoint (Frame3d properties) =
    properties.originPoint


xDirection : Frame3d -> Direction3d
xDirection (Frame3d properties) =
    properties.xDirection


yDirection : Frame3d -> Direction3d
yDirection (Frame3d properties) =
    properties.yDirection


zDirection : Frame3d -> Direction3d
zDirection (Frame3d properties) =
    properties.zDirection
