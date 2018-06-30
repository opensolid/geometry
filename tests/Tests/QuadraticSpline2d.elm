module Tests.QuadraticSpline2d exposing (..)

import Curve.ArcLengthParameterization as ArcLengthParameterization
import Curve.ParameterValue as ParameterValue
import Expect
import Float.Extra as Float
import Fuzz exposing (Fuzzer)
import Geometry.Fuzz as Fuzz
import Point2d
import QuadraticSpline2d exposing (QuadraticSpline2d)
import Test exposing (Test)


degenerateSpline : Fuzzer QuadraticSpline2d
degenerateSpline =
    Fuzz.point2d
        |> Fuzz.map
            (\point ->
                QuadraticSpline2d.with
                    { startPoint = point
                    , controlPoint = point
                    , endPoint = point
                    }
            )


curvedSpline : Fuzzer QuadraticSpline2d
curvedSpline =
    Fuzz.map5
        (\angle length interpolationParameter offset flipSide ->
            let
                x0 =
                    -length / 2

                x2 =
                    length / 2

                x1 =
                    Float.interpolateFrom x0 x2 interpolationParameter

                y1 =
                    if flipSide then
                        -offset
                    else
                        offset

                p0 =
                    Point2d.fromCoordinates ( x0, 0 )

                p1 =
                    Point2d.fromCoordinates ( x1, y1 )

                p2 =
                    Point2d.fromCoordinates ( x2, 0 )
            in
            QuadraticSpline2d.with
                { startPoint = p0
                , controlPoint = p1
                , endPoint = p2
                }
                |> QuadraticSpline2d.rotateAround Point2d.origin angle
        )
        (Fuzz.floatRange 0 (2 * pi))
        (Fuzz.floatRange 1 10)
        (Fuzz.floatRange 0 1)
        (Fuzz.floatRange 1 5)
        Fuzz.bool


analyticalLength : QuadraticSpline2d -> Float
analyticalLength spline =
    let
        p0 =
            QuadraticSpline2d.startPoint spline

        p1 =
            QuadraticSpline2d.controlPoint spline

        p2 =
            QuadraticSpline2d.endPoint spline

        ( x0, y0 ) =
            Point2d.coordinates p0

        ( x1, y1 ) =
            Point2d.coordinates p1

        ( x2, y2 ) =
            Point2d.coordinates p2

        ax =
            x0 - 2 * x1 + x2

        ay =
            y0 - 2 * y1 + y2

        bx =
            2 * x1 - 2 * x0

        by =
            2 * y1 - 2 * y0

        a =
            4 * (ax * ax + ay * ay)

        b =
            4 * (ax * bx + ay * by)

        c =
            bx * bx + by * by

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


exampleSpline : QuadraticSpline2d
exampleSpline =
    QuadraticSpline2d.with
        { startPoint = Point2d.fromCoordinates ( 1, 1 )
        , controlPoint = Point2d.fromCoordinates ( 3, 4 )
        , endPoint = Point2d.fromCoordinates ( 5, 1 )
        }


line : QuadraticSpline2d
line =
    QuadraticSpline2d.with
        { startPoint = Point2d.fromCoordinates ( 0, 1 )
        , controlPoint = Point2d.fromCoordinates ( 2.5, 1 )
        , endPoint = Point2d.fromCoordinates ( 5, 1 )
        }


parameterization : Test
parameterization =
    Test.describe "arc length parameterization"
        [ Test.test "at s = 0 the result is the starting point of the spline" <|
            \_ ->
                let
                    parameterizedCurve =
                        exampleSpline
                            |> QuadraticSpline2d.arcLengthParameterized
                                { maxError = 0.001 }
                in
                QuadraticSpline2d.pointAlong parameterizedCurve 0
                    |> Expect.equal (Just (Point2d.fromCoordinates ( 1, 1 )))
        , Test.fuzz Fuzz.quadraticSpline2d "at s = 0 the arcLengthParameterized curve gives the starting point" <|
            \spline ->
                let
                    parameterizedCurve =
                        spline
                            |> QuadraticSpline2d.arcLengthParameterized
                                { maxError = 0.001 }

                    startPoint =
                        QuadraticSpline2d.startPoint spline
                in
                QuadraticSpline2d.pointAlong parameterizedCurve 0
                    |> Expect.equal (Just startPoint)
        , Test.fuzz (Fuzz.oneOf [ Fuzz.quadraticSpline2d, degenerateSpline ]) "at s = (length spline) the arcLengthParameterized curve gives the end point" <|
            \spline ->
                let
                    parameterizedCurve =
                        spline
                            |> QuadraticSpline2d.arcLengthParameterized
                                { maxError = 0.001 }

                    endPoint =
                        QuadraticSpline2d.endPoint spline
                in
                QuadraticSpline2d.arcLength parameterizedCurve
                    |> QuadraticSpline2d.pointAlong parameterizedCurve
                    |> Expect.equal (Just endPoint)
        , Test.fuzz (Fuzz.oneOf [ Fuzz.quadraticSpline2d, degenerateSpline ]) "at s = (length spline) toParameterValue gives 1" <|
            \spline ->
                let
                    parameterizedCurve =
                        spline
                            |> QuadraticSpline2d.arcLengthParameterized
                                { maxError = 0.001 }

                    arcLength =
                        QuadraticSpline2d.arcLength parameterizedCurve

                    parameterValue =
                        parameterizedCurve
                            |> QuadraticSpline2d.arcLengthParameterization
                            |> ArcLengthParameterization.arcLengthToParameterValue
                                arcLength
                in
                if arcLength == 0 then
                    parameterValue |> Expect.equal (Just ParameterValue.zero)
                else
                    parameterValue |> Expect.equal (Just ParameterValue.one)
        , Test.fuzz curvedSpline "arc length matches analytical formula" <|
            -- analyticalLength falls down for degenerate splines so just check
            -- curved ones
            \spline ->
                let
                    tolerance =
                        0.001

                    parameterizedCurve =
                        spline
                            |> QuadraticSpline2d.arcLengthParameterized
                                { maxError = 0.001 }
                in
                QuadraticSpline2d.arcLength parameterizedCurve
                    |> Expect.within (Expect.Absolute tolerance)
                        (analyticalLength spline)
        , Test.test "at s = 5 the result is the end point of the line" <|
            \_ ->
                let
                    parameterizedCurve =
                        line
                            |> QuadraticSpline2d.arcLengthParameterized
                                { maxError = 0.1 }
                in
                QuadraticSpline2d.pointAlong parameterizedCurve 5
                    |> Expect.equal (Just (Point2d.fromCoordinates ( 5, 1 )))
        , Test.test "at s = 2.5 the result is the midpoint of the line" <|
            \_ ->
                let
                    parameterizedCurve =
                        line
                            |> QuadraticSpline2d.arcLengthParameterized
                                { maxError = 0.1 }
                in
                QuadraticSpline2d.pointAlong parameterizedCurve 2.5
                    |> Expect.equal (Just (Point2d.fromCoordinates ( 2.5, 1 )))
        , Test.test "at s=2.5 is about right" <|
            \_ ->
                let
                    parameterizedCurve =
                        exampleSpline
                            |> QuadraticSpline2d.arcLengthParameterized
                                { maxError = 0.001 }

                    expected =
                        Just <|
                            Point2d.fromCoordinates
                                ( 2.9008070813684963
                                , 2.4963102868350115
                                )
                in
                QuadraticSpline2d.pointAlong parameterizedCurve 2.5
                    |> Expect.equal expected
        , Test.test "length is as expected" <|
            \_ ->
                let
                    parameterizedCurve =
                        line
                            |> QuadraticSpline2d.arcLengthParameterized
                                { maxError = 0.001 }
                in
                QuadraticSpline2d.arcLength parameterizedCurve
                    |> Expect.equal 5
        , Test.test "length parameterization at approximate length is Just" <|
            \_ ->
                let
                    parameterizedCurve =
                        exampleSpline
                            |> QuadraticSpline2d.arcLengthParameterized
                                { maxError = 0.001 }

                    arcLength =
                        QuadraticSpline2d.arcLength parameterizedCurve
                in
                QuadraticSpline2d.pointAlong parameterizedCurve arcLength
                    |> Expect.equal (Just (Point2d.fromCoordinates ( 5, 1 )))
        ]
