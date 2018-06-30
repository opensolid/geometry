module EllipticalArc2d.With2 exposing (..)

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
            Direction2d.fromAngle (degrees 30)

        arc =
            EllipticalArc2d.with
                { centerPoint = centerPoint
                , xDirection = xDirection
                , xRadius = 200
                , yRadius = 100
                , startAngle = degrees -90
                , sweptAngle = degrees 180
                }

        svg =
            Svg.g [ blackStroke, whiteFill ]
                [ Svg.direction2d [] centerPoint xDirection
                , Svg.point2d [] centerPoint
                , Svg.ellipticalArc2d [ blackStroke, noFill ] arc
                , Svg.point2d [] (EllipticalArc2d.startPoint arc)
                , Svg.point2d [] (EllipticalArc2d.endPoint arc)
                ]

        ellipsePoints =
            List.range 0 100
                |> List.map
                    (\n -> EllipticalArc2d.pointOn arc (toFloat n / 100))

        { minX, maxX, minY, maxY } =
            List.map BoundingBox2d.singleton ellipsePoints
                |> List.foldl BoundingBox2d.hull
                    (BoundingBox2d.singleton centerPoint)
                |> BoundingBox2d.extrema

        padding =
            10

        bounds =
            BoundingBox2d.fromExtrema
                { minX = minX - padding
                , maxX = maxX + padding
                , minY = minY - padding
                , maxY = maxY + padding
                }
    in
    Svg.render2d bounds svg
