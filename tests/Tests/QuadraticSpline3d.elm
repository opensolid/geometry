module Tests.QuadraticSpline3d exposing (..)

import Expect exposing (FloatingPointTolerance(..))
import Fuzz exposing (Fuzzer)
import Geometry.Fuzz as Fuzz
import Point3d
import QuadraticSpline3d exposing (QuadraticSpline3d)
import Test exposing (Test)
import Tests.QuadraticSpline2d


analyticalLength : QuadraticSpline3d -> Float
analyticalLength spline =
    let
        p0 =
            QuadraticSpline3d.startPoint spline

        p1 =
            QuadraticSpline3d.controlPoint spline

        p2 =
            QuadraticSpline3d.endPoint spline

        ( x0, y0, z0 ) =
            Point3d.coordinates p0

        ( x1, y1, z1 ) =
            Point3d.coordinates p1

        ( x2, y2, z2 ) =
            Point3d.coordinates p2

        ax =
            x0 - 2 * x1 + x2

        ay =
            y0 - 2 * y1 + y2

        az =
            z0 - 2 * z1 + z2

        bx =
            2 * x1 - 2 * x0

        by =
            2 * y1 - 2 * y0

        bz =
            2 * z1 - 2 * z0

        a =
            4 * (ax * ax + ay * ay + az * az)

        b =
            4 * (ax * bx + ay * by + az * bz)

        c =
            bx * bx + by * by + bz * bz

        s_abc =
            2 * sqrt (a + b + c)

        a_2 =
            sqrt a

        a_32 =
            2 * a * a_2

        c_2 =
            2 * sqrt c

        ba =
            b / a_2
    in
    (a_32 * s_abc + a_2 * b * (s_abc - c_2) + (4 * c * a - b * b) * logBase e ((2 * a_2 + ba + s_abc) / (ba + c_2))) / (4 * a_32)


curvedSpline : Fuzzer QuadraticSpline3d
curvedSpline =
    Fuzz.map2 QuadraticSpline3d.on
        Fuzz.sketchPlane3d
        Tests.QuadraticSpline2d.curvedSpline


arcLengthMatchesAnalytical : Test
arcLengthMatchesAnalytical =
    Test.fuzz curvedSpline
        "arc length matches analytical formula"
        (\spline ->
            spline
                |> QuadraticSpline3d.arcLengthParameterized
                    { maxError = 1.0e-3 }
                |> QuadraticSpline3d.arcLength
                |> Expect.within (Absolute 1.0e-3) (analyticalLength spline)
        )


pointAtZeroLengthIsStart : Test
pointAtZeroLengthIsStart =
    Test.fuzz Fuzz.quadraticSpline3d
        "point along spline at zero length is start point"
        (\spline ->
            let
                parameterizedCurve =
                    spline
                        |> QuadraticSpline3d.arcLengthParameterized
                            { maxError = 1.0e-3 }
            in
            QuadraticSpline3d.pointAlong parameterizedCurve 0
                |> Expect.equal (Just (QuadraticSpline3d.startPoint spline))
        )


pointAtArcLengthIsEnd : Test
pointAtArcLengthIsEnd =
    Test.fuzz Fuzz.quadraticSpline3d
        "point along spline at arc length is end point"
        (\spline ->
            let
                parameterizedCurve =
                    spline
                        |> QuadraticSpline3d.arcLengthParameterized
                            { maxError = 1.0e-3 }

                arcLength =
                    QuadraticSpline3d.arcLength parameterizedCurve
            in
            QuadraticSpline3d.pointAlong parameterizedCurve arcLength
                |> Expect.equal (Just (QuadraticSpline3d.endPoint spline))
        )
