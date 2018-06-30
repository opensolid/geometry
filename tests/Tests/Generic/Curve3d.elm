module Tests.Generic.Curve3d exposing (Config, transformations)

import Axis3d exposing (Axis3d)
import Curve.ParameterValue as ParameterValue exposing (ParameterValue)
import Frame3d exposing (Frame3d)
import Fuzz exposing (Fuzzer)
import Geometry.Expect as Expect
import Geometry.Fuzz as Fuzz
import Plane3d exposing (Plane3d)
import Point3d exposing (Point3d)
import Test exposing (Test)
import Vector3d exposing (Vector3d)


type alias Config curve =
    { fuzzer : Fuzzer curve
    , pointOn : curve -> ParameterValue -> Point3d
    , firstDerivative : curve -> ParameterValue -> Vector3d
    , scaleAbout : Point3d -> Float -> curve -> curve
    , translateBy : Vector3d -> curve -> curve
    , rotateAround : Axis3d -> Float -> curve -> curve
    , mirrorAcross : Plane3d -> curve -> curve
    , relativeTo : Frame3d -> curve -> curve
    , placeIn : Frame3d -> curve -> curve
    }


transformations : Config curve -> Test
transformations config =
    Test.describe "Transformations"
        [ Test.fuzz4
            config.fuzzer
            Fuzz.point3d
            Fuzz.scalar
            Fuzz.parameterValue
            "scaleAbout"
            (\curve basePoint scale t ->
                let
                    scaledCurve =
                        config.scaleAbout basePoint scale curve

                    originalPoint =
                        config.pointOn curve t

                    pointOnScaledCurve =
                        config.pointOn scaledCurve t

                    scaledPoint =
                        originalPoint |> Point3d.scaleAbout basePoint scale
                in
                pointOnScaledCurve |> Expect.point3d scaledPoint
            )
        , Test.fuzz3
            config.fuzzer
            Fuzz.vector3d
            Fuzz.parameterValue
            "translateBy"
            (\curve displacement t ->
                let
                    translatedCurve =
                        config.translateBy displacement curve

                    originalPoint =
                        config.pointOn curve t

                    pointOnTranslatedCurve =
                        config.pointOn translatedCurve t

                    translatedPoint =
                        originalPoint |> Point3d.translateBy displacement
                in
                pointOnTranslatedCurve |> Expect.point3d translatedPoint
            )
        , Test.fuzz4
            config.fuzzer
            Fuzz.axis3d
            (Fuzz.floatRange (-2 * pi) (2 * pi))
            Fuzz.parameterValue
            "rotateAround"
            (\curve axis angle t ->
                let
                    rotatedCurve =
                        config.rotateAround axis angle curve

                    originalPoint =
                        config.pointOn curve t

                    pointOnRotatedCurve =
                        config.pointOn rotatedCurve t

                    rotatedPoint =
                        originalPoint |> Point3d.rotateAround axis angle
                in
                pointOnRotatedCurve |> Expect.point3d rotatedPoint
            )
        , Test.fuzz3
            config.fuzzer
            Fuzz.plane3d
            Fuzz.parameterValue
            "mirrorAcross"
            (\curve plane t ->
                let
                    mirroredCurve =
                        config.mirrorAcross plane curve

                    originalPoint =
                        config.pointOn curve t

                    pointOnMirroredCurve =
                        config.pointOn mirroredCurve t

                    mirroredPoint =
                        originalPoint |> Point3d.mirrorAcross plane
                in
                pointOnMirroredCurve |> Expect.point3d mirroredPoint
            )
        , Test.fuzz3
            config.fuzzer
            Fuzz.frame3d
            Fuzz.parameterValue
            "relativeTo"
            (\curve frame t ->
                let
                    localCurve =
                        config.relativeTo frame curve

                    originalPoint =
                        config.pointOn curve t

                    pointOnLocalCurve =
                        config.pointOn localCurve t

                    localPoint =
                        originalPoint |> Point3d.relativeTo frame
                in
                pointOnLocalCurve |> Expect.point3d localPoint
            )
        , Test.fuzz3
            config.fuzzer
            Fuzz.frame3d
            Fuzz.parameterValue
            "placeIn"
            (\curve frame t ->
                let
                    globalCurve =
                        config.placeIn frame curve

                    originalPoint =
                        config.pointOn curve t

                    pointOnGlobalCurve =
                        config.pointOn globalCurve t

                    globalPoint =
                        originalPoint |> Point3d.placeIn frame
                in
                pointOnGlobalCurve |> Expect.point3d globalPoint
            )
        ]
