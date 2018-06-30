module ReleaseNotes.DefaultParameterization exposing (..)

import CubicSpline2d
import Drawing2d
import Geometry.Parameter as Parameter
import Html exposing (Html)
import ReleaseNotes.Common exposing (..)


main : Html Never
main =
    let
        points =
            CubicSpline2d.pointsOn spline (Parameter.numSteps numSegments)
    in
    Drawing2d.toHtml renderBounds
        []
        [ Drawing2d.cubicSpline spline
        , Drawing2d.dots points
        ]
