module Tests.Circle2d exposing (..)

import BoundingBox2d
import Circle2d
import Expect
import Geometry.Fuzz as Fuzz
import Test exposing (Test)


boundingBoxContainsCenter : Test
boundingBoxContainsCenter =
    Test.fuzz Fuzz.circle2d
        "A circle's bounding box contains its center point"
        (\circle ->
            let
                boundingBox =
                    Circle2d.boundingBox circle

                centerPoint =
                    Circle2d.centerPoint circle
            in
            Expect.true
                "Circle bounding box does not contain the center point"
                (BoundingBox2d.contains centerPoint boundingBox)
        )
