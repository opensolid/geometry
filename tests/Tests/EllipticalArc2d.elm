module Tests.EllipticalArc2d exposing (..)

import Arc.SweptAngle as SweptAngle
import Arc2d exposing (Arc2d)
import EllipticalArc2d exposing (EllipticalArc2d)
import Expect
import Fuzz exposing (Fuzzer)
import Geometry.Expect as Expect
import Geometry.Fuzz as Fuzz
import Point2d
import Test exposing (Test)
import Tests.Generic.Curve2d
import Vector2d


reproducibleArc : Fuzzer Arc2d
reproducibleArc =
    Fuzz.map4
        (\centerPoint startDirection radius sweptAngle ->
            let
                startPoint =
                    centerPoint
                        |> Point2d.translateBy
                            (Vector2d.withLength radius startDirection)
            in
            startPoint |> Arc2d.sweptAround centerPoint sweptAngle
        )
        Fuzz.point2d
        Fuzz.direction2d
        (Fuzz.floatRange 0.1 10)
        (Fuzz.oneOf
            [ Fuzz.floatRange (degrees 1) (degrees 179)
            , Fuzz.floatRange (degrees 181) (degrees 359)
            , Fuzz.floatRange (degrees -179) (degrees -1)
            , Fuzz.floatRange (degrees -359) (degrees -181)
            ]
        )


fromEndpointsReplicatesArc : Test
fromEndpointsReplicatesArc =
    Test.fuzz2
        reproducibleArc
        Fuzz.direction2d
        "fromEndpoints accurately replicates circular arcs"
        (\arc xDirection ->
            let
                radius =
                    Arc2d.radius arc

                arcSweptAngle =
                    Arc2d.sweptAngle arc

                sweptAngle =
                    if arcSweptAngle >= pi then
                        SweptAngle.largePositive
                    else if arcSweptAngle >= 0 then
                        SweptAngle.smallPositive
                    else if arcSweptAngle >= -pi then
                        SweptAngle.smallNegative
                    else
                        SweptAngle.largeNegative

                result =
                    EllipticalArc2d.fromEndpoints
                        { startPoint = Arc2d.startPoint arc
                        , endPoint = Arc2d.endPoint arc
                        , xRadius = radius
                        , yRadius = radius
                        , xDirection = xDirection
                        , sweptAngle = sweptAngle
                        }
            in
            case result of
                Nothing ->
                    Expect.fail "fromEndpoints could not reproduce arc"

                Just ellipticalArc ->
                    EllipticalArc2d.centerPoint ellipticalArc
                        |> Expect.point2d (Arc2d.centerPoint arc)
        )


transformations : Test
transformations =
    Tests.Generic.Curve2d.transformations
        { fuzzer = Fuzz.ellipticalArc2d
        , pointOn = EllipticalArc2d.pointOn
        , firstDerivative = EllipticalArc2d.firstDerivative
        , scaleAbout = EllipticalArc2d.scaleAbout
        , translateBy = EllipticalArc2d.translateBy
        , rotateAround = EllipticalArc2d.rotateAround
        , mirrorAcross = EllipticalArc2d.mirrorAcross
        , relativeTo = EllipticalArc2d.relativeTo
        , placeIn = EllipticalArc2d.placeIn
        }
