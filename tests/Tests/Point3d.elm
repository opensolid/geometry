module Tests.Point3d exposing (..)

import Direction3d
import Expect
import Fuzz
import Geometry.Expect as Expect
import Geometry.Fuzz as Fuzz
import Plane3d
import Point3d
import SketchPlane3d
import Test exposing (Test)
import Vector3d


rotationAboutAxisPreservesDistanceAlong : Test
rotationAboutAxisPreservesDistanceAlong =
    let
        description =
            "Rotation around an axis preserves distance along that axis"

        expectation point axis angle =
            let
                distance =
                    Point3d.signedDistanceAlong axis point

                rotatedPoint =
                    Point3d.rotateAround axis angle point

                rotatedDistance =
                    Point3d.signedDistanceAlong axis rotatedPoint
            in
            Expect.approximately distance rotatedDistance
    in
    Test.fuzz3 Fuzz.point3d Fuzz.axis3d Fuzz.scalar description expectation


rotationAboutAxisPreservesDistanceFrom : Test
rotationAboutAxisPreservesDistanceFrom =
    let
        description =
            "Rotation around an axis preserves distance from that axis"

        expectation point axis angle =
            let
                distance =
                    Point3d.distanceFromAxis axis point

                rotatedPoint =
                    Point3d.rotateAround axis angle point

                rotatedDistance =
                    Point3d.distanceFromAxis axis rotatedPoint
            in
            Expect.approximately distance rotatedDistance
    in
    Test.fuzz3 Fuzz.point3d Fuzz.axis3d Fuzz.scalar description expectation


midpointIsEquidistant : Test
midpointIsEquidistant =
    Test.fuzz2
        Fuzz.point3d
        Fuzz.point3d
        "Midpoint of two points is equidistant from those points"
        (\p1 p2 ->
            let
                midpoint =
                    Point3d.midpoint p1 p2
            in
            Expect.approximately
                (Point3d.distanceFrom p1 midpoint)
                (Point3d.distanceFrom p2 midpoint)
        )


interpolationReturnsExactEndpoints : Test
interpolationReturnsExactEndpoints =
    Test.fuzz (Fuzz.tuple ( Fuzz.point3d, Fuzz.point3d ))
        "Interpolation returns exact start point for t=0 and exact end point for t=1"
        (Expect.all
            [ \( p1, p2 ) -> Point3d.interpolateFrom p1 p2 0 |> Expect.equal p1
            , \( p1, p2 ) -> Point3d.interpolateFrom p1 p2 1 |> Expect.equal p2
            ]
        )


projectIntoThenPlaceOntoIsProjectOnto : Test
projectIntoThenPlaceOntoIsProjectOnto =
    Test.fuzz2 Fuzz.point3d
        Fuzz.sketchPlane3d
        "Point3d.projectInto followed by Point3d.on is equivalent to Point3d.projectOnto"
        (\point sketchPlane ->
            let
                plane =
                    SketchPlane3d.toPlane sketchPlane
            in
            Point3d.projectInto sketchPlane point
                |> Point3d.on sketchPlane
                |> Expect.point3d (Point3d.projectOnto plane point)
        )


mirrorFlipsSignedDistance : Test
mirrorFlipsSignedDistance =
    Test.fuzz2 Fuzz.point3d
        Fuzz.plane3d
        "'mirrorAcross plane' changes the sign of the 'signedDistanceFrom plane'"
        (\point plane ->
            let
                signedDistance =
                    Point3d.signedDistanceFrom plane point
            in
            Point3d.mirrorAcross plane point
                |> Point3d.signedDistanceFrom plane
                |> Expect.approximately -signedDistance
        )


translationByPerpendicularDoesNotChangeSignedDistance : Test
translationByPerpendicularDoesNotChangeSignedDistance =
    Test.fuzz3 Fuzz.point3d
        Fuzz.plane3d
        Fuzz.scalar
        "Translating in a direction perpendicular to a plane's normal direction does not change signed distance from that plane"
        (\point plane distance ->
            let
                originalSignedDistance =
                    Point3d.signedDistanceFrom plane point

                normalDirection =
                    Plane3d.normalDirection plane

                displacement =
                    Vector3d.withLength distance
                        (Direction3d.perpendicularTo normalDirection)
            in
            Point3d.translateBy displacement point
                |> Point3d.signedDistanceFrom plane
                |> Expect.approximately originalSignedDistance
        )


translateByAndInAreConsistent : Test
translateByAndInAreConsistent =
    Test.fuzz3
        Fuzz.point3d
        Fuzz.direction3d
        Fuzz.scalar
        "translateBy and translateIn are consistent"
        (\point direction distance ->
            let
                displacement =
                    Vector3d.withLength distance direction
            in
            point
                |> Point3d.translateIn direction distance
                |> Expect.point3d (Point3d.translateBy displacement point)
        )
