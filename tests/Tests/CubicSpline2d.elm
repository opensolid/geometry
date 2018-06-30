module Tests.CubicSpline2d exposing (..)

import CubicSpline2d
import Expect exposing (FloatingPointTolerance(..))
import Geometry.Expect as Expect
import Geometry.Fuzz as Fuzz
import Test exposing (Test)
import Tests.QuadraticSpline2d


fromEndpointsReproducesSpline : Test
fromEndpointsReproducesSpline =
    Test.fuzz Fuzz.cubicSpline2d
        "CubicSpline2d.fromEndpoints reproduces original spline"
        (\spline ->
            let
                startPoint =
                    CubicSpline2d.startPoint spline

                endPoint =
                    CubicSpline2d.endPoint spline

                startDerivative =
                    CubicSpline2d.startDerivative spline

                endDerivative =
                    CubicSpline2d.endDerivative spline
            in
            CubicSpline2d.fromEndpoints
                { startPoint = startPoint
                , startDerivative = startDerivative
                , endPoint = endPoint
                , endDerivative = endDerivative
                }
                |> Expect.cubicSpline2d spline
        )


arcLengthMatchesAnalytical : Test
arcLengthMatchesAnalytical =
    Test.fuzz Tests.QuadraticSpline2d.curvedSpline
        "arc length matches analytical formula"
        (\quadraticSpline ->
            quadraticSpline
                |> CubicSpline2d.fromQuadraticSpline
                |> CubicSpline2d.arcLengthParameterized { maxError = 1.0e-3 }
                |> CubicSpline2d.arcLength
                |> Expect.within (Absolute 1.0e-3)
                    (Tests.QuadraticSpline2d.analyticalLength quadraticSpline)
        )


pointAtZeroLengthIsStart : Test
pointAtZeroLengthIsStart =
    Test.fuzz Fuzz.cubicSpline2d
        "point along spline at zero length is start point"
        (\spline ->
            let
                parameterizedCurve =
                    spline
                        |> CubicSpline2d.arcLengthParameterized
                            { maxError = 1.0e-3 }
            in
            CubicSpline2d.pointAlong parameterizedCurve 0
                |> Expect.equal (Just (CubicSpline2d.startPoint spline))
        )


pointAtArcLengthIsEnd : Test
pointAtArcLengthIsEnd =
    Test.fuzz Fuzz.cubicSpline2d
        "point along spline at arc length is end point"
        (\spline ->
            let
                parameterizedCurve =
                    spline
                        |> CubicSpline2d.arcLengthParameterized
                            { maxError = 1.0e-3 }

                arcLength =
                    CubicSpline2d.arcLength parameterizedCurve
            in
            CubicSpline2d.pointAlong parameterizedCurve arcLength
                |> Expect.equal (Just (CubicSpline2d.endPoint spline))
        )



--derivativeAndMagnitudeAreConsistent : Test
--derivativeAndMagnitudeAreConsistent =
--    Test.fuzz2
--        Fuzz.cubicSpline2d
--        (Fuzz.floatRange 0 1)
--        "derivative and derivativeMagnitude are consistent"
--        (\spline t ->
--            CubicSpline2d.derivative spline t
--                |> Maybe.map Vector2d.length
--                |> Expect.just Expect.approximately
--                    (CubicSpline2d.derivativeMagnitude spline t)
--        )
