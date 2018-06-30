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


{-| Construct a 3D arc lying _on_ a sketch plane by providing a 2D arc specified
in XY coordinates _within_ the sketch plane.

    arc =
        Arc3d.on SketchPlane3d.xz
            (Point2d.fromCoordinates ( 3, 1 )
                |> Arc2d.sweptAround
                    (Point2d.fromCoordinates ( 1, 1 )
                    (degrees 90)
            )

    Arc3d.centerPoint arc
    --> Point3d.fromCoordinates ( 1, 0, 1 )

    Arc3d.radius arc
    --> 2

    Arc3d.startPoint arc
    --> Point3d.fromCoordinates ( 3, 0, 1 )

    Arc3d.endPoint arc
    --> Point3d.fromCoordinates ( 1, 0, 3 )

-}
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


{-| Construct an arc by sweeping the given point around the given axis by the
given angle:

    exampleArc =
        Point3d.fromCoordinates ( 1, 1, 0 )
            |> Arc3d.sweptAround Axis3d.z (degrees 90)

    Arc3d.centerPoint exampleArc
    --> Point3d.origin

    Arc3d.endPoint exampleArc
    --> Point3d.fromCoordinates ( -1, 1, 0 )

Positive swept angles result in a counterclockwise (right-handed) rotation
around the given axis and vice versa for negative swept angles. The center point
of the returned arc will lie on the given axis.

-}
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


{-| Attempt to construct an arc that starts at the first given point, passes
through the second given point and ends at the third given point. If the three
points are collinear, returns `Nothing`.

    p1 =
        Point3d.fromCoordinates ( 0, 0, 1 )

    p2 =
        Point3d.origin

    p3 =
        Point3d.fromCoordinates ( 0, 1, 0 )

    Arc3d.throughPoints p1 p2 p3
    --> Just
    -->     (Arc3d.on SketchPlane3d.yz
    -->         Point2d.fromCoordinates ( 0, 1 )
    -->             |> Arc2d.sweptAround
    -->                 (Point2d.fromCoordinates
    -->                     ( 0.5, 0.5 )
    -->                 )
    -->                 (degrees 180)
    -->     )

-}
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


{-| Get the axial direction of an arc.

    Arc3d.axialDirection exampleArc
    --> Direction3d.z

-}
axialDirection : Arc3d -> Direction3d
axialDirection (Types.Arc3d arc) =
    let
        axialDirectionVector =
            Vector3d.crossProduct
                (Direction3d.toVector arc.xDirection)
                (Direction3d.toVector arc.yDirection)
    in
    Direction3d.unsafe (Vector3d.components axialDirectionVector)


{-| Get the central axis of an arc. The origin point of the axis will be equal
to the center point of the arc.

    Arc3d.axis exampleArc
    --> Axis3d.z

-}
axis : Arc3d -> Axis3d
axis arc =
    Axis3d.through (centerPoint arc) (axialDirection arc)


{-| Get the center point of an arc.

    Arc3d.centerPoint exampleArc
    --> Point3d.origin

-}
centerPoint : Arc3d -> Point3d
centerPoint (Types.Arc3d arc) =
    let
        radius_ =
            arc.signedLength / arc.sweptAngle
    in
    arc.startPoint |> Point3d.translateIn arc.yDirection radius_


{-| Get the radius of an arc.

    Arc3d.radius exampleArc
    --> 1.4142

-}
radius : Arc3d -> Float
radius (Types.Arc3d arc) =
    arc.signedLength / arc.sweptAngle


{-| Get the start point of an arc.

    Arc3d.startPoint exampleArc
    --> Point3d.fromCoordinates ( 1, 1, 0 )

-}
startPoint : Arc3d -> Point3d
startPoint (Types.Arc3d arc) =
    arc.startPoint


{-| Get the end point of an arc.

    Arc3d.endPoint exampleArc
    --> Point3d.fromCoordinates ( -1, 1, 0 )

-}
endPoint : Arc3d -> Point3d
endPoint arc =
    pointOn arc ParameterValue.one


{-| Get the point along an arc at a given parameter value:

    Arc3d.pointOn exampleArc ParameterValue.half
    --> Point3d.fromCoordinates ( 0, 1.4142, 0 )

-}
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


{-| Get points along an arc at a given set of parameter values.

    exampleArc |> Arc3d.pointsAt (ParameterValue.steps 2)
    --> [ Point3d ( 1, 1, 0 )
    --> , Point3d ( 0, 1.4142, 0 )
    --> , Point3d ( -1, 1, 0 )
    --> ]

-}
pointsAt : List ParameterValue -> Arc3d -> List Point3d
pointsAt parameterValues arc =
    List.map (pointOn arc) parameterValues


{-| Get the first derivative of an arc at a given parameter value.

    Arc3d.firstDerivative exampleArc ParameterValue.zero
    --> Vector3d.fromComponents ( -1.5708, 1.5708, 0 )

    Arc3d.firstDerivative exampleArc ParameterValue.one
    --> Vector3d.fromComponents ( -1.5708, -1.5708, 0 )

-}
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


{-| Evaluate the first derivative of an arc at a range of parameter values.

    exampleArc
        |> Arc3d.firstDerivativesAt
            (ParameterValue.steps 2)
    --> [ Vector3d ( -1.5708, 1.5708, 0 )
    --> , Vector3d ( -2.2214, 0, 0 )
    --> , Vector3d ( -1.5708, -1.5708, 0 )
    --> ]

-}
firstDerivativesAt : List ParameterValue -> Arc3d -> List Vector3d
firstDerivativesAt parameterValues arc =
    List.map (firstDerivative arc) parameterValues


{-| If a curve has zero length (consists of just a single point), then we say
that it is 'degenerate'. Some operations such as computing tangent directions
are not defined on degenerate curves.

A `Nondegenerate` value represents an arc that is definitely not degenerate. It
is used as input to functions such as `Arc3d.tangentDirection` and can be
constructed using `Arc3d.nondegenerate`.

-}
type Nondegenerate
    = Nondegenerate Arc3d


{-| Attempt to construct a nondegenerate arc from a general `Arc3d`. If the arc
is in fact degenerate (consists of a single point), returns an `Err` with that
point.

    Arc3d.nondegenerate exampleArc
    --> Ok nondegenerateExampleArc

-}
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


{-| Convert a nondegenerate arc back to a general `Arc3d`.

    Arc3d.fromNondegenerate nondegenerateExampleArc
    --> exampleArc

-}
fromNondegenerate : Nondegenerate -> Arc3d
fromNondegenerate (Nondegenerate arc) =
    arc


{-| Get the tangent direction to a nondegenerate arc at a given parameter
value:

    Arc3d.tangentDirection nondegenerateExampleArc
        ParameterValue.zero
    --> Direction3d.fromAzimuthAndElevation
    -->     (degrees 135)
    -->     (degrees 0)

    Arc3d.tangentDirection nondegenerateExampleArc
        ParameterValue.half
    --> Direction3d.negativeX

    Arc3d.tangentDirection nondegenerateExampleArc
        ParameterValue.zero
    --> Direction3d.fromAzimuthAndElevation
    -->     (degrees 225)
    -->     (degrees 0)

-}
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


{-| Get tangent directions to a nondegenerate arc at a given set of parameter
values:

    nondegenerateExampleArc
        |> Arc3d.tangentDirectionsAt
            (ParameterValue.steps 2)
    --> [ Direction3d.fromAzimuthAndElevation
    -->     (degrees 135)
    -->     (degrees 0)
    --> , Direction3d.negativeX
    --> , Direction3d.fromAzimuthAndElevation
    -->     (degrees 225)
    -->     (degrees 0)
    --> ]

-}
tangentDirectionsAt : List ParameterValue -> Nondegenerate -> List Direction3d
tangentDirectionsAt parameterValues nondegenerateArc =
    List.map (tangentDirection nondegenerateArc) parameterValues


{-| Get both the point and tangent direction of a nondegenerate arc at a given
parameter value:

    Arc3d.sample nondegenerateExampleArc
        ParameterValue.zero
    --> ( Point3d.fromCoordinates ( 1, 1, 0 )
    --> , Direction3d.fromAzimuthAndElevation
    -->     (degrees 135)
    -->     (degrees 0)
    --> )

    Arc3d.sample nondegenerateExampleArc
        ParameterValue.half
    --> ( Point3d.fromCoordinates ( 0, 1.4142, 0 )
    --> , Direction3d.negativeX
    --> )

    Arc3d.sample nondegenerateExampleArc
        ParameterValue.one
    --> ( Point3d.fromCoordinates ( -1, 1, 0 )
    --> , Direction3d.fromAzimuthAndElevation
    -->     (degrees 225)
    -->     (degrees 0)
    --> )

-}
sample : Nondegenerate -> ParameterValue -> ( Point3d, Direction3d )
sample nondegenerateArc parameterValue =
    ( pointOn (fromNondegenerate nondegenerateArc) parameterValue
    , tangentDirection nondegenerateArc parameterValue
    )


{-| Get points and tangent directions of a nondegenerate arc at a given set of
parameter values:

    nondegenerateExampleArc
        |> Arc3d.samplesAt (ParameterValue.steps 2)
    --> [ ( Point3d.fromCoordinates ( 1, 1, 0 )
    -->   , Direction3d.fromAzimuthAndElevation
    -->         (degrees 135)
    -->         (degrees 0)
    -->   )
    --> , ( Point3d.fromCoordinates ( 0, 1.4142, 0 )
    -->   , Direction3d.negativeX
    -->   )
    --> , ( Point3d.fromCoordinates ( -1, 1, 0 )
    -->   , Direction3d.fromAzimuthAndElevation
    -->         (degrees 225)
    -->         (degrees 0)
    -->   )
    --> ]

If the arc is degenerate (start point and end point are equal), returns an
empty list.

-}
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


{-| Approximate an arc as a polyline, within a given tolerance:

    exampleArc |> Arc3d.toPolyline { maxError = 0.1 }
    --> Polyline3d.fromVertices
    -->     [ Point3d.fromCoordinates ( 1, 1, 0 )
    -->     , Point3d.fromCoordinates ( 0.366, 1.366, 0 )
    -->     , Point3d.fromCoordinates ( -0.366, 1.366, 0 )
    -->     , Point3d.fromCoordinates ( -1, 1, 0 )
    -->     ]

In this example, every point on the returned polyline will be within 0.1 units
of the original arc.

-}
toPolyline : { maxError : Float } -> Arc3d -> Polyline3d
toPolyline { maxError } arc =
    let
        numSegments =
            numApproximationSegments maxError arc

        points =
            arc |> pointsAt (ParameterValue.steps numSegments)
    in
    Polyline3d.fromVertices points


{-| Get the swept angle of an arc in radians.

    Arc3d.sweptAngle exampleArc
    --> 1.5708

A positive swept angle means that the arc is formed by rotating the given start
point counterclockwise around the central axis, and vice versa for a negative
angle.

-}
sweptAngle : Arc3d -> Float
sweptAngle (Types.Arc3d properties) =
    properties.sweptAngle


{-| Reverse the direction of an arc, so that the start point becomes the end
point and vice versa. The resulting arc will have the same axis as the original
but a swept angle with the opposite sign.

    Arc3d.reverse exampleArc
    --> Arc3d.sweptAround Axis3d.z
    -->     (degrees -90)
    -->     (Point3d.fromCoordinates ( -1, 1, 0 ))

-}
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


{-| Scale an arc about the given center point by the given scale.

    point =
        Point3d.fromCoordinates ( 0, -1, 0 )

    Arc3d.scaleAbout point 2 exampleArc
    --> Arc3d.sweptAround
    -->     (Axis3d.withDirection Direction3d.z
    -->         (Point3d.fromCoordinates ( 0, 1, 0 ))
    -->     )
    -->     (degrees 90)
    -->     (Point3d.fromCoordinates ( 2, 3, 0 ))

-}
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


{-| Rotate an arc around a given axis by a given angle (in radians).

    Arc3d.rotateAround Axis3d.x (degrees 90) exampleArc
    --> Arc3d.sweptAround (Axis3d.reverse Axis3d.y)
    -->     (degrees 90)
    -->     (Point3d.fromCoordinates ( 1, 0, 1 ))

-}
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


{-| Translate an arc by a given displacement.

    displacement =
        Vector3d.fromComponents ( 2, 1, 3 )

    Arc3d.translateBy displacement exampleArc
    --> Arc3d.sweptAround
    -->     (Axis3d.withDirection Direction3d.z
    -->         (Point3d ( 2, 1, 3 ))
    -->     )
    -->     (degrees 90)
    -->     (Point3d.fromCoordinates ( 3, 2, 3 ))

-}
translateBy : Vector3d -> Arc3d -> Arc3d
translateBy displacement (Types.Arc3d arc) =
    Types.Arc3d
        { startPoint = Point3d.translateBy displacement arc.startPoint
        , sweptAngle = arc.sweptAngle
        , signedLength = arc.signedLength
        , xDirection = arc.xDirection
        , yDirection = arc.yDirection
        }


{-| Translate an arc in a given direction by a given distance;

    Arc3d.translateIn direction distance

is equivalent to

    Arc3d.translateBy
        (Vector3d.withLength distance direction)

-}
translateIn : Direction3d -> Float -> Arc3d -> Arc3d
translateIn direction distance arc =
    translateBy (Vector3d.withLength distance direction) arc


{-| Mirror an arc across a given plane.

    Arc3d.mirrorAcross Plane3d.xy exampleArc
    --> Arc3d.sweptAround (Axis3d.reverse Axis3d.z)
    -->     (degrees -90)
    -->     (Point3d.fromCoordinates ( 1, 1, 0 ))

Note that this flips the sign of the arc's swept angle.

-}
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


{-| Project an arc into a sketch plane.

    axis : Axis3d
    axis =
        Axis3d.through
            (Point3d.fromCoordinates ( 1, 2, 3 ))
            (Direction3d.fromAzimuthAndElevation
                (degrees 0)
                (degrees 45)
            )

    arc : Arc3d
    arc =
        Arc3d.sweptAround axis
            (degrees 45)
            (Point3d.fromCoordinates ( 1, 4, 3 ))

    Arc3d.projectInto SketchPlane3d.xy arc
    --> EllipticalArc2d.with
    -->     { centerPoint =
    -->         Point2d.fromCoordinates ( 1, 2 )
    -->     , xDirection = Direction2d.y
    -->     , xRadius = 2
    -->     , yRadius = 1.4142
    -->     , startAngle = degrees 0
    -->     , sweptAngle = degrees 45
    -->     }

-}
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


{-| Take an arc defined in global coordinates, and return it expressed in local
coordinates relative to a given reference frame.

    localFrame =
        Frame3d.atPoint
            (Point3d.fromCoordinates ( 1, 2, 3 ))

    Arc3d.relativeTo localFrame exampleArc
    --> Arc3d.sweptAround
    -->     (Axis3d.withDirection Direction3d.z
    -->         (Point3d ( -1, -2, -3 ))
    -->     )
    -->     (degrees 90)
    -->     (Point3d.fromCoordinates ( 0, -1, -3 ))

-}
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


{-| Take an arc considered to be defined in local coordinates relative to a
given reference frame, and return that arc expressed in global coordinates.

    localFrame =
        Frame3d.atPoint
            (Point3d.fromCoordinates ( 1, 2, 3 ))

    Arc3d.placeIn localFrame exampleArc
    --> Arc3d.sweptAround
    -->     (Axis3d.withDirection Direction3d.z
    -->         (Point3d.fromCoordinates ( 1, 2, 3 ))
    -->     )
    -->     (degrees 90)
    -->     (Point3d.fromCoordinates ( 2, 3, 3 ))

-}
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
