module Tests.Direction3d exposing (..)

import Direction3d
import Expect
import Frame3d
import Fuzz
import Geometry.Expect as Expect
import Geometry.Fuzz as Fuzz
import Point3d
import Test exposing (Test)
import Vector3d


angleFromAndEqualWithinAreConsistent : Test
angleFromAndEqualWithinAreConsistent =
    Test.fuzz2 Fuzz.direction3d
        Fuzz.direction3d
        "angleFrom and equalWithin are consistent"
        (\firstDirection secondDirection ->
            let
                angle =
                    Direction3d.angleFrom firstDirection secondDirection
            in
            Expect.true "Two directions should be equal to within the angle between them"
                (Direction3d.equalWithin (angle + 1.0e-12)
                    firstDirection
                    secondDirection
                )
        )


orthonormalizeProducesValidFrameBasis : Test
orthonormalizeProducesValidFrameBasis =
    Test.fuzz (Fuzz.tuple3 ( Fuzz.vector3d, Fuzz.vector3d, Fuzz.vector3d ))
        "orthonormalize produces a valid frame basis"
        (\( v1, v2, v3 ) ->
            let
                tripleProduct =
                    Vector3d.crossProduct v1 v2
                        |> Vector3d.dotProduct v3
            in
            if abs tripleProduct > 1.0e-6 then
                case Direction3d.orthonormalize v1 v2 v3 of
                    Just ( xDirection, yDirection, zDirection ) ->
                        Expect.validFrame3d
                            (Frame3d.unsafe
                                { originPoint = Point3d.origin
                                , xDirection = xDirection
                                , yDirection = yDirection
                                , zDirection = zDirection
                                }
                            )

                    Nothing ->
                        Expect.fail "Could not orthonormalize valid set of vectors"
            else
                Expect.pass
        )


orthonormalizeFollowsOriginalVectors : Test
orthonormalizeFollowsOriginalVectors =
    Test.fuzz (Fuzz.tuple3 ( Fuzz.vector3d, Fuzz.vector3d, Fuzz.vector3d ))
        "orthonormalized directions follow original vectors properly"
        (\( v1, v2, v3 ) ->
            case Direction3d.orthonormalize v1 v2 v3 of
                Just directions ->
                    directions
                        |> Expect.all
                            [ \( xDirection, _, _ ) ->
                                Vector3d.componentIn xDirection v1
                                    |> Expect.greaterThan 0
                            , \( _, yDirection, _ ) ->
                                Vector3d.componentIn yDirection v1
                                    |> Expect.approximately 0
                            , \( _, _, zDirection ) ->
                                Vector3d.componentIn zDirection v1
                                    |> Expect.approximately 0
                            , \( _, yDirection, _ ) ->
                                Vector3d.componentIn yDirection v2
                                    |> Expect.greaterThan 0
                            , \( _, _, zDirection ) ->
                                Vector3d.componentIn zDirection v2
                                    |> Expect.approximately 0
                            , \( _, _, zDirection ) ->
                                Vector3d.componentIn zDirection v3
                                    |> Expect.greaterThan 0
                            ]

                Nothing ->
                    Expect.pass
        )


orthonormalizingCoplanarVectorsReturnsNothing : Test
orthonormalizingCoplanarVectorsReturnsNothing =
    Test.test "orthonormalizing coplanar vectors returns Nothing"
        (\() ->
            let
                v1 =
                    Vector3d.fromComponents ( 1, 0, 0 )

                v2 =
                    Vector3d.fromComponents ( 2, 3, 0 )

                v3 =
                    Vector3d.fromComponents ( -1, 2, 0 )
            in
            Expect.equal Nothing (Direction3d.orthonormalize v1 v2 v3)
        )
