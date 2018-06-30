module Tests.Axis3d exposing (..)

import Axis2d
import Axis3d
import Direction2d
import Direction3d
import Geometry.Expect as Expect
import Geometry.Fuzz as Fuzz
import Point2d
import Point3d
import SketchPlane3d
import Test exposing (Test)


xExample : Test
xExample =
    Test.test "Axis3d.x example" <|
        \() ->
            Axis3d.x
                |> Expect.axis3d (Axis3d.through Point3d.origin Direction3d.x)


yExample : Test
yExample =
    Test.test "Axis3d.y example" <|
        \() ->
            Axis3d.y
                |> Expect.axis3d (Axis3d.through Point3d.origin Direction3d.y)


zExample : Test
zExample =
    Test.test "Axis3d.z example" <|
        \() ->
            Axis3d.z
                |> Expect.axis3d (Axis3d.through Point3d.origin Direction3d.z)


originPointExample : Test
originPointExample =
    Test.test "Axis3d.originPoint example" <|
        \() ->
            Axis3d.originPoint Axis3d.x |> Expect.point3d Point3d.origin


directionExample : Test
directionExample =
    Test.test "Axis3d.direction example" <|
        \() ->
            Axis3d.direction Axis3d.y |> Expect.direction3d Direction3d.y


onExamples : Test
onExamples =
    let
        axis2d =
            Axis2d.through
                (Point2d.fromCoordinates ( 1, 3 ))
                (Direction2d.fromAngle (degrees 30))
    in
    Test.describe "Axis3d.on examples"
        [ Test.test "First example" <|
            \() ->
                Axis3d.on SketchPlane3d.xy axis2d
                    |> Expect.axis3d
                        (Axis3d.through
                            (Point3d.fromCoordinates ( 1, 3, 0 ))
                            (Direction3d.fromAzimuthAndElevation
                                (degrees 30)
                                (degrees 0)
                            )
                        )
        , Test.test "Second example" <|
            \() ->
                Axis3d.on SketchPlane3d.zx axis2d
                    |> Expect.axis3d
                        (Axis3d.through
                            (Point3d.fromCoordinates ( 3, 0, 1 ))
                            (Direction3d.fromAzimuthAndElevation
                                (degrees 0)
                                (degrees 60)
                            )
                        )
        ]
