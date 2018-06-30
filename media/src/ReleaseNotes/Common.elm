module ReleaseNotes.Common exposing (..)

import BoundingBox2d exposing (BoundingBox2d)
import CubicSpline2d exposing (CubicSpline2d)
import Point2d exposing (Point2d)


spline : CubicSpline2d
spline =
    CubicSpline2d.fromControlPoints
        ( Point2d.fromCoordinates ( 100, 100 )
        , Point2d.fromCoordinates ( 250, 300 )
        , Point2d.fromCoordinates ( 150, 0 )
        , Point2d.fromCoordinates ( 300, 200 )
        )


numSegments : Int
numSegments =
    24


renderBounds : BoundingBox2d
renderBounds =
    BoundingBox2d.fromExtrema
        { minX = 80
        , maxX = 320
        , minY = 80
        , maxY = 220
        }
