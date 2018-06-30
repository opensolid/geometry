module EllipticalArc2d.With1 exposing (..)

import BoundingBox2d exposing (BoundingBox2d)
import Direction2d exposing (Direction2d)
import EllipticalArc2d exposing (EllipticalArc2d)
import FillsAndStrokes exposing (..)
import Html exposing (Html)
import Point2d exposing (Point2d)
import Svg exposing (Svg)


main : Html Never
main =
    let
        centerPoint =
            Point2d.origin

        xDirection =
            Direction2d.x

        arc =
            EllipticalArc2d.with
                { centerPoint = centerPoint
                , xDirection = xDirection
                , xRadius = 200
                , yRadius = 100
                , startAngle = 0
                , sweptAngle = degrees 90
                }

        svg =
            Svg.g [ blackStroke, whiteFill ]
                [ Svg.direction2d [] centerPoint xDirection
                , Svg.point2d [] centerPoint
                , Svg.ellipticalArc2d [ blackStroke, noFill ] arc
                , Svg.point2d [] (EllipticalArc2d.startPoint arc)
                , Svg.point2d [] (EllipticalArc2d.endPoint arc)
                ]

        bounds =
            BoundingBox2d.fromExtrema
                { minX = -10
                , maxX = 210
                , minY = -10
                , maxY = 110
                }
    in
    Svg.render2d bounds svg
