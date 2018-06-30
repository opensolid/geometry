module Tests.Direction2d exposing (..)

import Direction2d
import Expect
import Frame2d
import Fuzz
import Geometry.Expect as Expect
import Geometry.Fuzz as Fuzz
import Point2d
import Test exposing (Test)
import Vector2d


angleFromAndEqualWithinAreConsistent : Test
angleFromAndEqualWithinAreConsistent =
    Test.fuzz2 Fuzz.direction2d
        Fuzz.direction2d
        "angleFrom and equalWithin are consistent"
        (\firstDirection secondDirection ->
            let
                angle =
                    abs (Direction2d.angleFrom firstDirection secondDirection)
            in
            Expect.true "Two directions should be equal to within the angle between them"
                (Direction2d.equalWithin (angle + 1.0e-12)
                    firstDirection
                    secondDirection
                )
        )


angleFromAndRotateByAreConsistent : Test
angleFromAndRotateByAreConsistent =
    Test.fuzz2 Fuzz.direction2d
        Fuzz.direction2d
        "angleFrom and rotateBy are consistent"
        (\firstDirection secondDirection ->
            let
                angle =
                    Direction2d.angleFrom firstDirection secondDirection
            in
            firstDirection
                |> Direction2d.rotateBy angle
                |> Expect.direction2d secondDirection
        )


orthonormalizeProducesValidFrameBasis : Test
orthonormalizeProducesValidFrameBasis =
    Test.fuzz (Fuzz.tuple ( Fuzz.vector2d, Fuzz.vector2d ))
        "orthonormalize produces a valid frame basis"
        (\( xVector, yVector ) ->
            case Direction2d.orthonormalize xVector yVector of
                Just ( xDirection, yDirection ) ->
                    Expect.validFrame2d
                        (Frame2d.unsafe
                            { originPoint = Point2d.origin
                            , xDirection = xDirection
                            , yDirection = yDirection
                            }
                        )

                Nothing ->
                    let
                        crossProduct =
                            Vector2d.crossProduct xVector yVector
                    in
                    Expect.approximately 0.0 crossProduct
        )


orthonormalizingParallelVectorsReturnsNothing : Test
orthonormalizingParallelVectorsReturnsNothing =
    Test.test "orthonormalizing parallel vectors returns Nothing"
        (\() ->
            let
                xVector =
                    Vector2d.fromComponents ( 1, 2 )

                yVector =
                    Vector2d.fromComponents ( -3, -6 )
            in
            Expect.equal Nothing (Direction2d.orthonormalize xVector yVector)
        )
