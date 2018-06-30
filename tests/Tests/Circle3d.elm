module Tests.Circle3d exposing (..)

import BoundingBox3d
import Circle3d
import Expect
import Geometry.Fuzz as Fuzz
import Point3d
import Test exposing (Test)
import Triangle3d


throughPoints : Test
throughPoints =
    Test.describe "throughPoints"
        [ Test.fuzz3 Fuzz.point3d
            Fuzz.point3d
            Fuzz.point3d
            "All given points lie on the circle constructed using `throughPoints`"
            (\p1 p2 p3 ->
                let
                    -- if the first three points are collinear it is not possible to construct a circle
                    -- that passes through them.
                    -- three points are collinear if the area of the triangle they form is zero.
                    isValidInput =
                        let
                            triangleArea =
                                Triangle3d.fromVertices ( p1, p2, p3 )
                                    |> Triangle3d.area
                        in
                        triangleArea > 1.0e-6

                    maybeCircle =
                        Circle3d.throughPoints p1 p2 p3

                    liesOnCircle point circle =
                        Point3d.distanceFrom point (Circle3d.centerPoint circle)
                            |> Expect.within (Expect.Absolute 1.0e-6) (Circle3d.radius circle)
                in
                case maybeCircle of
                    Just circle ->
                        Expect.all (List.map liesOnCircle [ p1, p2, p3 ]) circle

                    Nothing ->
                        if isValidInput then
                            Expect.fail "throughPoints returned Nothing on valid input"
                        else
                            Expect.pass
            )
        ]


boundingBoxContainsCenter : Test
boundingBoxContainsCenter =
    Test.fuzz Fuzz.circle3d
        "A circle's bounding box contains its center point"
        (\circle ->
            let
                boundingBox =
                    Circle3d.boundingBox circle

                centerPoint =
                    Circle3d.centerPoint circle
            in
            Expect.true
                "Circle bounding box does not contain the center point"
                (BoundingBox3d.contains centerPoint boundingBox)
        )
