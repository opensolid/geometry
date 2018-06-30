module Tests.Frame3d exposing (..)

import Direction3d
import Frame3d
import Geometry.Expect as Expect
import Geometry.Fuzz as Fuzz
import Test exposing (Test)
import Vector3d


frameDirectionsAreOrthonormal : Test
frameDirectionsAreOrthonormal =
    Test.fuzz Fuzz.frame3d
        "Frame3d basis directions are orthonormal"
        (\frame ->
            let
                xDirectionVector =
                    Direction3d.toVector (Frame3d.xDirection frame)

                yDirectionVector =
                    Direction3d.toVector (Frame3d.yDirection frame)

                zDirectionVector =
                    Direction3d.toVector (Frame3d.zDirection frame)

                tripleProduct =
                    Vector3d.crossProduct xDirectionVector yDirectionVector
                        |> Vector3d.dotProduct zDirectionVector

                expectedTripleProduct =
                    if Frame3d.isRightHanded frame then
                        1
                    else
                        -1
            in
            Expect.approximately expectedTripleProduct tripleProduct
        )
