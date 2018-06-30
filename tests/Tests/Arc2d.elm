module Tests.Arc2d exposing (..)

import Arc.SweptAngle as SweptAngle exposing (SweptAngle)
import Arc2d
import Curve.ParameterValue as ParameterValue exposing (ParameterValue)
import Expect
import Fuzz
import Geometry.Expect as Expect
import Geometry.Fuzz as Fuzz
import Point2d exposing (Point2d)
import Test exposing (Test)
import Tests.Generic.Curve2d


evaluateZeroIsStartPoint : Test
evaluateZeroIsStartPoint =
    Test.fuzz Fuzz.arc2d
        "Evaluating at t=0 returns start point"
        (\arc ->
            Arc2d.pointOn arc ParameterValue.zero
                |> Expect.point2d (Arc2d.startPoint arc)
        )


evaluateOneIsEndPoint : Test
evaluateOneIsEndPoint =
    Test.fuzz Fuzz.arc2d
        "Evaluating at t=1 returns end point"
        (\arc ->
            Arc2d.pointOn arc ParameterValue.one
                |> Expect.point2d (Arc2d.endPoint arc)
        )


reverseFlipsDirection : Test
reverseFlipsDirection =
    Test.fuzz2 Fuzz.arc2d
        Fuzz.parameterValue
        "Reversing an arc is consistent with reversed evaluation"
        (\arc parameterValue ->
            Arc2d.pointOn (Arc2d.reverse arc) parameterValue
                |> Expect.point2d
                    (Arc2d.pointOn arc (ParameterValue.oneMinus parameterValue))
        )


from : Test
from =
    let
        validAngle =
            Fuzz.oneOf
                [ Fuzz.floatRange (degrees -359) (degrees 359)
                , Fuzz.floatRange (degrees 361) (degrees 719)
                , Fuzz.floatRange (degrees -719) (degrees -361)
                ]
    in
    Test.fuzz3
        Fuzz.point2d
        Fuzz.point2d
        validAngle
        "Arc2d.from produces the expected end point and swept angle"
        (\startPoint endPoint sweptAngle ->
            Arc2d.from startPoint endPoint sweptAngle
                |> Expect.all
                    [ Arc2d.endPoint >> Expect.point2d endPoint
                    , Arc2d.sweptAngle >> Expect.approximately sweptAngle
                    ]
        )


withRadius : Test
withRadius =
    let
        sweptAngleTypes =
            List.map Fuzz.constant
                [ SweptAngle.smallPositive
                , SweptAngle.smallNegative
                , SweptAngle.largePositive
                , SweptAngle.largeNegative
                ]
    in
    Test.fuzz4
        (Fuzz.map abs Fuzz.scalar)
        (Fuzz.oneOf sweptAngleTypes)
        Fuzz.point2d
        Fuzz.point2d
        "Arc2d.withRadius produces the expected end point"
        (\radius sweptAngleType startPoint endPoint ->
            case Arc2d.withRadius radius sweptAngleType startPoint endPoint of
                Just arc ->
                    arc |> Arc2d.endPoint |> Expect.point2d endPoint

                Nothing ->
                    let
                        distance =
                            Point2d.distanceFrom startPoint endPoint
                    in
                    if distance == 0 then
                        Expect.pass
                    else
                        distance |> Expect.greaterThan (2 * radius)
        )


transformations : Test
transformations =
    Tests.Generic.Curve2d.transformations
        { fuzzer = Fuzz.arc2d
        , pointOn = Arc2d.pointOn
        , firstDerivative = Arc2d.firstDerivative
        , scaleAbout = Arc2d.scaleAbout
        , translateBy = Arc2d.translateBy
        , rotateAround = Arc2d.rotateAround
        , mirrorAcross = Arc2d.mirrorAcross
        , relativeTo = Arc2d.relativeTo
        , placeIn = Arc2d.placeIn
        }


mirroredCenterPoint : Test
mirroredCenterPoint =
    Test.fuzz2
        Fuzz.arc2d
        Fuzz.axis2d
        "Center point of a mirrored finite-radius arc is the mirror of the original center point"
        (\arc axis ->
            if Arc2d.radius arc < 100 then
                let
                    mirroredArc =
                        Arc2d.mirrorAcross axis arc
                in
                Arc2d.centerPoint mirroredArc
                    |> Expect.point2d
                        (Point2d.mirrorAcross axis (Arc2d.centerPoint arc))
            else
                Expect.pass
        )
