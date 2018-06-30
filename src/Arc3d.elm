module Arc3d
    exposing
        ( Arc3d
        , Nondegenerate
        , axialDirection
        , axis
        , centerPoint
        , endPoint
        , firstDerivative
        , firstDerivativesAt
        , fromNondegenerate
        , mirrorAcross
        , nondegenerate
        , on
        , placeIn
        , pointOn
        , pointsAt
        , projectInto
        , radius
        , relativeTo
        , reverse
        , rotateAround
        , sample
        , samplesAt
        , scaleAbout
        , startPoint
        , sweptAngle
        , sweptAround
        , tangentDirection
        , tangentDirectionsAt
        , throughPoints
        , toPolyline
        , translateBy
        , translateIn
        )

{-| <img src="https://ianmackenzie.github.io/elm-geometry/1.0.0/Arc3d/icon.svg" alt="Arc3d" width="160">

An `Arc3d` is a section of a circle in 3D, defined by its central axis,
start point and swept angle (the counterclockwise angle around the axis from the
start point to the arc's end point). This module includes functionality for

  - Constructing arcs through given points
  - Scaling, rotating, translating and mirroring arcs
  - Converting arcs between different coordinate systems

@docs Arc3d


# Constructors

@docs on, sweptAround, throughPoints


# Properties

@docs axialDirection, axis, centerPoint, radius, startPoint, endPoint, sweptAngle


# Evaluation

@docs pointOn, pointsAt
@docs Nondegenerate, nondegenerate, fromNondegenerate
@docs tangentDirection, tangentDirectionsAt, sample, samplesAt


# Linear approximation

@docs toPolyline


# Transformations

@docs reverse, scaleAbout, rotateAround, translateBy, translateIn, mirrorAcross, projectInto


# Coordinate conversions

@docs relativeTo, placeIn


# Differentiation

You are unlikely to need to use these functions directly, but they are useful if
you are writing low-level geometric algorithms.

@docs firstDerivative, firstDerivativesAt

-}

import Arc2d exposing (Arc2d)
import Axis3d exposing (Axis3d)
import Curve.ParameterValue as ParameterValue exposing (ParameterValue)
import Direction2d exposing (Direction2d)
import Direction3d exposing (Direction3d)
import Frame2d exposing (Frame2d)
import Frame3d exposing (Frame3d)
import Geometry.Types as Types
import Plane3d exposing (Plane3d)
import Point3d exposing (Point3d)
import Polyline3d exposing (Polyline3d)
import SketchPlane3d exposing (SketchPlane3d)
import Vector3d exposing (Vector3d)


{-| -}
type alias Arc3d =
    Types.Arc3d


{-| -}
on : SketchPlane3d -> Arc2d -> Arc3d
on sketchPlane (Types.Arc2d arc2d) =
    Types.Arc3d
        { startPoint = Point3d.on sketchPlane arc2d.startPoint
        , xDirection = Direction3d.on sketchPlane arc2d.xDirection
        , yDirection =
            Direction3d.on sketchPlane
                (Direction2d.perpendicularTo arc2d.xDirection)
        , sweptAngle = arc2d.sweptAngle
        , signedLength = arc2d.signedLength
        }


{-| -}
sweptAround : Axis3d -> Float -> Point3d -> Arc3d
sweptAround axis_ sweptAngle_ startPoint_ =
    let
        centerPoint_ =
            startPoint_ |> Point3d.projectOntoAxis axis_

        axisDirection =
            Axis3d.direction axis_
    in
    case Vector3d.lengthAndDirection (Vector3d.from startPoint_ centerPoint_) of
        Just ( radius_, yDirection ) ->
            let
                xDirectionVector =
                    Vector3d.crossProduct
                        (Direction3d.toVector yDirection)
                        (Direction3d.toVector axisDirection)

                xDirection =
                    Direction3d.unsafe (Vector3d.components xDirectionVector)
            in
            Types.Arc3d
                { startPoint = startPoint_
                , sweptAngle = sweptAngle_
                , signedLength = radius_ * sweptAngle_
                , xDirection = xDirection
                , yDirection = yDirection
                }

        Nothing ->
            let
                ( xDirection, yDirection ) =
                    Direction3d.perpendicularBasis axisDirection
            in
            Types.Arc3d
                { startPoint = startPoint_
                , sweptAngle = sweptAngle_
                , signedLength = 0
                , xDirection = xDirection
                , yDirection = yDirection
                }


{-| -}
throughPoints : Point3d -> Point3d -> Point3d -> Maybe Arc3d
throughPoints firstPoint secondPoint thirdPoint =
    SketchPlane3d.throughPoints firstPoint secondPoint thirdPoint
        |> Maybe.andThen
            (\sketchPlane ->
                Arc2d.throughPoints
                    (Point3d.projectInto sketchPlane firstPoint)
                    (Point3d.projectInto sketchPlane secondPoint)
                    (Point3d.projectInto sketchPlane thirdPoint)
                    |> Maybe.map (on sketchPlane)
            )


{-| -}
axialDirection : Arc3d -> Direction3d
axialDirection (Types.Arc3d arc) =
    let
        axialDirectionVector =
            Vector3d.crossProduct
                (Direction3d.toVector arc.xDirection)
                (Direction3d.toVector arc.yDirection)
    in
    Direction3d.unsafe (Vector3d.components axialDirectionVector)


{-| -}
axis : Arc3d -> Axis3d
axis arc =
    Axis3d.through (centerPoint arc) (axialDirection arc)


{-| -}
centerPoint : Arc3d -> Point3d
centerPoint (Types.Arc3d arc) =
    let
        radius_ =
            arc.signedLength / arc.sweptAngle
    in
    arc.startPoint |> Point3d.translateIn arc.yDirection radius_


{-| -}
radius : Arc3d -> Float
radius (Types.Arc3d arc) =
    arc.signedLength / arc.sweptAngle


{-| -}
startPoint : Arc3d -> Point3d
startPoint (Types.Arc3d arc) =
    arc.startPoint


{-| -}
endPoint : Arc3d -> Point3d
endPoint arc =
    pointOn arc ParameterValue.one


{-| -}
pointOn : Arc3d -> ParameterValue -> Point3d
pointOn (Types.Arc3d arc) parameterValue =
    let
        ( x0, y0, z0 ) =
            Point3d.coordinates arc.startPoint

        ( x1, y1, z1 ) =
            Direction3d.components arc.xDirection

        ( x2, y2, z2 ) =
            Direction3d.components arc.yDirection

        arcSignedLength =
            arc.signedLength

        arcSweptAngle =
            arc.sweptAngle

        t =
            ParameterValue.value parameterValue
    in
    if arcSweptAngle == 0.0 then
        let
            distance =
                t * arcSignedLength
        in
        Point3d.fromCoordinates
            ( x0 + distance * x1
            , y0 + distance * y1
            , z0 + distance * z1
            )
    else
        let
            theta =
                t * arcSweptAngle

            arcRadius =
                arcSignedLength / arcSweptAngle

            x =
                arcRadius * sin theta

            y =
                if abs theta < pi / 2 then
                    x * tan (theta / 2)
                else
                    arcRadius * (1 - cos theta)
        in
        Point3d.fromCoordinates
            ( x0 + x * x1 + y * x2
            , y0 + x * y1 + y * y2
            , z0 + x * z1 + y * z2
            )


{-| -}
pointsAt : List ParameterValue -> Arc3d -> List Point3d
pointsAt parameterValues arc =
    List.map (pointOn arc) parameterValues


{-| -}
firstDerivative : Arc3d -> ParameterValue -> Vector3d
firstDerivative (Types.Arc3d arc) =
    let
        ( x1, y1, z1 ) =
            Direction3d.components arc.xDirection

        ( x2, y2, z2 ) =
            Direction3d.components arc.yDirection

        arcSweptAngle =
            arc.sweptAngle

        arcSignedLength =
            arc.signedLength
    in
    \parameterValue ->
        let
            t =
                ParameterValue.value parameterValue

            angle =
                t * arcSweptAngle

            cosAngle =
                cos angle

            sinAngle =
                sin angle
        in
        Vector3d.fromComponents
            ( arcSignedLength * (cosAngle * x1 + sinAngle * x2)
            , arcSignedLength * (cosAngle * y1 + sinAngle * y2)
            , arcSignedLength * (cosAngle * z1 + sinAngle * z2)
            )


{-| -}
firstDerivativesAt : List ParameterValue -> Arc3d -> List Vector3d
firstDerivativesAt parameterValues arc =
    List.map (firstDerivative arc) parameterValues


{-| -}
type Nondegenerate
    = Nondegenerate Arc3d


{-| -}
nondegenerate : Arc3d -> Result Point3d Nondegenerate
nondegenerate arc =
    let
        (Types.Arc3d properties) =
            arc
    in
    if properties.signedLength == 0 then
        Err (startPoint arc)
    else
        Ok (Nondegenerate arc)


{-| -}
fromNondegenerate : Nondegenerate -> Arc3d
fromNondegenerate (Nondegenerate arc) =
    arc


{-| -}
tangentDirection : Nondegenerate -> ParameterValue -> Direction3d
tangentDirection (Nondegenerate (Types.Arc3d arc)) parameterValue =
    let
        ( x1, y1, z1 ) =
            Direction3d.components arc.xDirection

        ( x2, y2, z2 ) =
            Direction3d.components arc.yDirection

        arcSweptAngle =
            arc.sweptAngle

        t =
            ParameterValue.value parameterValue

        angle =
            t * arcSweptAngle

        cosAngle =
            cos angle

        sinAngle =
            sin angle
    in
    Direction3d.unsafe
        ( cosAngle * x1 + sinAngle * x2
        , cosAngle * y1 + sinAngle * y2
        , cosAngle * z1 + sinAngle * z2
        )


{-| -}
tangentDirectionsAt : List ParameterValue -> Nondegenerate -> List Direction3d
tangentDirectionsAt parameterValues nondegenerateArc =
    List.map (tangentDirection nondegenerateArc) parameterValues


{-| -}
sample : Nondegenerate -> ParameterValue -> ( Point3d, Direction3d )
sample nondegenerateArc parameterValue =
    ( pointOn (fromNondegenerate nondegenerateArc) parameterValue
    , tangentDirection nondegenerateArc parameterValue
    )


{-| -}
samplesAt : List ParameterValue -> Nondegenerate -> List ( Point3d, Direction3d )
samplesAt parameterValues nondegenerateArc =
    List.map (sample nondegenerateArc) parameterValues


numApproximationSegments : Float -> Arc3d -> Int
numApproximationSegments maxError arc =
    if sweptAngle arc == 0 then
        1
    else if maxError <= 0 then
        0
    else if maxError >= 2 * radius arc then
        1
    else
        let
            maxSegmentAngle =
                2 * acos (1 - maxError / radius arc)
        in
        ceiling (abs (sweptAngle arc) / maxSegmentAngle)


{-| -}
toPolyline : { maxError : Float } -> Arc3d -> Polyline3d
toPolyline { maxError } arc =
    let
        numSegments =
            numApproximationSegments maxError arc

        points =
            arc |> pointsAt (ParameterValue.steps numSegments)
    in
    Polyline3d.fromVertices points


{-| -}
sweptAngle : Arc3d -> Float
sweptAngle (Types.Arc3d properties) =
    properties.sweptAngle


{-| -}
reverse : Arc3d -> Arc3d
reverse ((Types.Arc3d arc) as arc_) =
    let
        ( x1, y1, z1 ) =
            Direction3d.components arc.xDirection

        ( x2, y2, z2 ) =
            Direction3d.components arc.yDirection

        arcSweptAngle =
            arc.sweptAngle

        cosAngle =
            cos arcSweptAngle

        sinAngle =
            sin arcSweptAngle
    in
    Types.Arc3d
        { startPoint = endPoint arc_
        , sweptAngle = -arcSweptAngle
        , signedLength = -arc.signedLength
        , xDirection =
            Direction3d.unsafe
                ( x1 * cosAngle + x2 * sinAngle
                , y1 * cosAngle + y2 * sinAngle
                , z1 * cosAngle + z2 * sinAngle
                )
        , yDirection =
            Direction3d.unsafe
                ( x2 * cosAngle - x1 * sinAngle
                , y2 * cosAngle - y1 * sinAngle
                , z2 * cosAngle - z1 * sinAngle
                )
        }


{-| -}
scaleAbout : Point3d -> Float -> Arc3d -> Arc3d
scaleAbout point scale (Types.Arc3d arc) =
    Types.Arc3d
        { startPoint = Point3d.scaleAbout point scale arc.startPoint
        , sweptAngle = arc.sweptAngle
        , signedLength = abs scale * arc.signedLength
        , xDirection =
            if scale >= 0 then
                arc.xDirection
            else
                Direction3d.reverse arc.xDirection
        , yDirection =
            if scale >= 0 then
                arc.yDirection
            else
                Direction3d.reverse arc.yDirection
        }


{-| -}
rotateAround : Axis3d -> Float -> Arc3d -> Arc3d
rotateAround rotationAxis angle =
    let
        rotatePoint =
            Point3d.rotateAround rotationAxis angle

        rotateDirection =
            Direction3d.rotateAround rotationAxis angle
    in
    \(Types.Arc3d arc) ->
        Types.Arc3d
            { startPoint = rotatePoint arc.startPoint
            , sweptAngle = arc.sweptAngle
            , signedLength = arc.signedLength
            , xDirection = rotateDirection arc.xDirection
            , yDirection = rotateDirection arc.yDirection
            }


{-| -}
translateBy : Vector3d -> Arc3d -> Arc3d
translateBy displacement (Types.Arc3d arc) =
    Types.Arc3d
        { startPoint = Point3d.translateBy displacement arc.startPoint
        , sweptAngle = arc.sweptAngle
        , signedLength = arc.signedLength
        , xDirection = arc.xDirection
        , yDirection = arc.yDirection
        }


{-| -}
translateIn : Direction3d -> Float -> Arc3d -> Arc3d
translateIn direction distance arc =
    translateBy (Vector3d.withLength distance direction) arc


{-| -}
mirrorAcross : Plane3d -> Arc3d -> Arc3d
mirrorAcross plane =
    let
        mirrorPoint =
            Point3d.mirrorAcross plane

        mirrorDirection =
            Direction3d.mirrorAcross plane
    in
    \(Types.Arc3d arc) ->
        Types.Arc3d
            { startPoint = mirrorPoint arc.startPoint
            , sweptAngle = -arc.sweptAngle
            , signedLength = -arc.signedLength
            , xDirection = Direction3d.reverse (mirrorDirection arc.xDirection)
            , yDirection = mirrorDirection arc.yDirection
            }


{-| -}
projectInto : SketchPlane3d -> Arc3d -> Types.EllipticalArc2d
projectInto sketchPlane arc =
    let
        candidateXDirection2d =
            case Direction3d.projectInto sketchPlane (axialDirection arc) of
                Just yDirection2d ->
                    yDirection2d |> Direction2d.rotateClockwise

                Nothing ->
                    Direction2d.x

        candidateXDirection3d =
            Direction3d.on sketchPlane candidateXDirection2d

        radialVector =
            Vector3d.from (centerPoint arc) (startPoint arc)

        ( xDirection2d, xDirection3d ) =
            if Vector3d.componentIn candidateXDirection3d radialVector >= 0 then
                ( candidateXDirection2d
                , candidateXDirection3d
                )
            else
                ( Direction2d.reverse candidateXDirection2d
                , Direction3d.reverse candidateXDirection3d
                )

        arcRadius =
            radius arc

        normalComponent =
            axialDirection arc
                |> Direction3d.componentIn
                    (SketchPlane3d.normalDirection sketchPlane)

        yRatio =
            abs normalComponent

        ellipticalStartAngle =
            let
                xVector =
                    Direction3d.toVector xDirection3d

                crossProduct =
                    Vector3d.crossProduct xVector radialVector

                y =
                    crossProduct
                        |> Vector3d.componentIn (axialDirection arc)

                x =
                    Vector3d.dotProduct radialVector xVector

                arcStartAngle =
                    atan2 y x
            in
            if normalComponent >= 0 then
                arcStartAngle
            else
                -arcStartAngle

        ellipticalSweptAngle =
            if normalComponent >= 0 then
                sweptAngle arc
            else
                -(sweptAngle arc)
    in
    Types.EllipticalArc2d
        { ellipse =
            Types.Ellipse2d
                { axes =
                    Frame2d.withXDirection xDirection2d
                        (centerPoint arc |> Point3d.projectInto sketchPlane)
                , xRadius = arcRadius
                , yRadius = arcRadius * yRatio
                }
        , startAngle = ellipticalStartAngle
        , sweptAngle = ellipticalSweptAngle
        }


{-| -}
relativeTo : Frame3d -> Arc3d -> Arc3d
relativeTo frame (Types.Arc3d arc) =
    if Frame3d.isRightHanded frame then
        Types.Arc3d
            { startPoint = Point3d.relativeTo frame arc.startPoint
            , sweptAngle = arc.sweptAngle
            , signedLength = arc.signedLength
            , xDirection = Direction3d.relativeTo frame arc.xDirection
            , yDirection = Direction3d.relativeTo frame arc.yDirection
            }
    else
        Types.Arc3d
            { startPoint = Point3d.relativeTo frame arc.startPoint
            , sweptAngle = -arc.sweptAngle
            , signedLength = -arc.signedLength
            , xDirection =
                Direction3d.relativeTo frame arc.xDirection
                    |> Direction3d.reverse
            , yDirection = Direction3d.relativeTo frame arc.yDirection
            }


{-| -}
placeIn : Frame3d -> Arc3d -> Arc3d
placeIn frame (Types.Arc3d arc) =
    if Frame3d.isRightHanded frame then
        Types.Arc3d
            { startPoint = Point3d.placeIn frame arc.startPoint
            , sweptAngle = arc.sweptAngle
            , signedLength = arc.signedLength
            , xDirection = Direction3d.placeIn frame arc.xDirection
            , yDirection = Direction3d.placeIn frame arc.yDirection
            }
    else
        Types.Arc3d
            { startPoint = Point3d.placeIn frame arc.startPoint
            , sweptAngle = -arc.sweptAngle
            , signedLength = -arc.signedLength
            , xDirection =
                Direction3d.placeIn frame arc.xDirection
                    |> Direction3d.reverse
            , yDirection = Direction3d.placeIn frame arc.yDirection
            }
