module Tests.Generic.Curve2d exposing (Config, transformations)

import Axis2d exposing (Axis2d)
import Curve.ParameterValue as ParameterValue exposing (ParameterValue)
import Frame2d exposing (Frame2d)
import Fuzz exposing (Fuzzer)
import Geometry.Expect as Expect
import Geometry.Fuzz as Fuzz
import Point2d exposing (Point2d)
import Test exposing (Test)
import Vector2d exposing (Vector2d)


type alias Config curve =
    { fuzzer : Fuzzer curve
    , pointOn : curve -> ParameterValue -> Point2d
    , firstDerivative : curve -> ParameterValue -> Vector2d
    , scaleAbout : Point2d -> Float -> curve -> curve
    , translateBy : Vector2d -> curve -> curve
    , rotateAround : Point2d -> Float -> curve -> curve
    , mirrorAcross : Axis2d -> curve -> curve
    , relativeTo : Frame2d -> curve -> curve
    , placeIn : Frame2d -> curve -> curve
    }


transformations : Config curve -> Test
transformations config =
    Test.describe "Transformations"
        [ Test.describe "scaleAbout"
            [ Test.fuzz4
                config.fuzzer
                Fuzz.point2d
                Fuzz.scalar
                Fuzz.parameterValue
                "position"
                (\curve basePoint scale t ->
                    let
                        scaledCurve =
                            config.scaleAbout basePoint scale curve

                        originalPoint =
                            config.pointOn curve t

                        pointOnScaledCurve =
                            config.pointOn scaledCurve t

                        scaledPoint =
                            originalPoint |> Point2d.scaleAbout basePoint scale
                    in
                    pointOnScaledCurve |> Expect.point2d scaledPoint
                )
            , Test.fuzz4
                config.fuzzer
                Fuzz.point2d
                Fuzz.scalar
                Fuzz.parameterValue
                "firstDerivative"
                (\curve basePoint scale t ->
                    let
                        scaledCurve =
                            config.scaleAbout basePoint scale curve

                        originalDerivative =
                            config.firstDerivative curve t

                        derivativeOfScaledCurve =
                            config.firstDerivative scaledCurve t

                        scaledDerivative =
                            originalDerivative |> Vector2d.scaleBy scale
                    in
                    derivativeOfScaledCurve |> Expect.vector2d scaledDerivative
                )
            ]
        , Test.fuzz3
            config.fuzzer
            Fuzz.vector2d
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
                        originalPoint |> Point2d.translateBy displacement
                in
                pointOnTranslatedCurve |> Expect.point2d translatedPoint
            )
        , Test.fuzz4
            config.fuzzer
            Fuzz.point2d
            (Fuzz.floatRange (-2 * pi) (2 * pi))
            Fuzz.parameterValue
            "rotateAround"
            (\curve centerPoint angle t ->
                let
                    rotatedCurve =
                        config.rotateAround centerPoint angle curve

                    originalPoint =
                        config.pointOn curve t

                    pointOnRotatedCurve =
                        config.pointOn rotatedCurve t

                    rotatedPoint =
                        originalPoint |> Point2d.rotateAround centerPoint angle
                in
                pointOnRotatedCurve |> Expect.point2d rotatedPoint
            )
        , Test.fuzz3
            config.fuzzer
            Fuzz.axis2d
            Fuzz.parameterValue
            "mirrorAcross"
            (\curve axis t ->
                let
                    mirroredCurve =
                        config.mirrorAcross axis curve

                    originalPoint =
                        config.pointOn curve t

                    pointOnMirroredCurve =
                        config.pointOn mirroredCurve t

                    mirroredPoint =
                        originalPoint |> Point2d.mirrorAcross axis
                in
                pointOnMirroredCurve |> Expect.point2d mirroredPoint
            )
        , Test.fuzz3
            config.fuzzer
            Fuzz.frame2d
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
                        originalPoint |> Point2d.relativeTo frame
                in
                pointOnLocalCurve |> Expect.point2d localPoint
            )
        , Test.fuzz3
            config.fuzzer
            Fuzz.frame2d
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
                        originalPoint |> Point2d.placeIn frame
                in
                pointOnGlobalCurve |> Expect.point2d globalPoint
            )
        ]
