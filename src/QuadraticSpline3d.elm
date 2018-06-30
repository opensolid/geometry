module QuadraticSpline3d
    exposing
        ( ArcLengthParameterized
        , Nondegenerate
        , QuadraticSpline3d
        , arcLength
        , arcLengthParameterization
        , arcLengthParameterized
        , bisect
        , boundingBox
        , controlPoint
        , endDerivative
        , endPoint
        , firstDerivative
        , firstDerivativesAt
        , fromArcLengthParameterized
        , fromNondegenerate
        , mirrorAcross
        , nondegenerate
        , on
        , placeIn
        , pointAlong
        , pointOn
        , pointsAt
        , projectInto
        , projectOnto
        , relativeTo
        , reverse
        , rotateAround
        , sample
        , sampleAlong
        , samplesAt
        , scaleAbout
        , secondDerivative
        , splitAt
        , startDerivative
        , startPoint
        , tangentDirection
        , tangentDirectionAlong
        , tangentDirectionsAt
        , translateBy
        , translateIn
        , with
        )

{-| <img src="https://ianmackenzie.github.io/elm-geometry/1.0.0/QuadraticSpline3d/icon.svg" alt="QuadraticSpline3d" width="160">

A `QuadraticSpline3d` is a quadratic [Bézier curve](https://en.wikipedia.org/wiki/B%C3%A9zier_curve)
in 3D defined by a start point, control point and end point. This module
contains functionality for

  - Evaluating points and derivatives along a spline
  - Scaling, rotating, translating or mirroring a spline
  - Converting a spline between local and global coordinates in different
    reference frames

@docs QuadraticSpline3d


# Constructors

@docs with, on


# Properties

@docs startPoint, endPoint, controlPoint, startDerivative, endDerivative, boundingBox


# Evaluation

@docs pointOn, pointsAt
@docs Nondegenerate, nondegenerate, fromNondegenerate
@docs tangentDirection, tangentDirectionsAt, sample, samplesAt


# Transformations

@docs reverse, scaleAbout, rotateAround, translateBy, translateIn, mirrorAcross, projectOnto


# Coordinate conversions

@docs relativeTo, placeIn


# Sketch planes

@docs projectInto


# Subdivision

@docs bisect, splitAt


# Arc length parameterization

@docs ArcLengthParameterized, arcLengthParameterized, arcLength, pointAlong, tangentDirectionAlong, sampleAlong


## Low level

An `ArcLengthParameterized` value is a combination of an
[`ArcLengthParameterization`](Geometry-ArcLengthParameterization) and an
underlying `QuadraticSpline3d`. If you need to do something fancy, you can
extract these two values separately.

@docs arcLengthParameterization, fromArcLengthParameterized


# Differentiation

You are unlikely to need to use these functions directly, but they are useful if
you are writing low-level geometric algorithms.

@docs firstDerivative, firstDerivativesAt, secondDerivative

-}

import Axis3d exposing (Axis3d)
import BoundingBox3d exposing (BoundingBox3d)
import Curve.ArcLengthParameterization as ArcLengthParameterization exposing (ArcLengthParameterization)
import Curve.ParameterValue as ParameterValue exposing (ParameterValue)
import Direction3d exposing (Direction3d)
import Frame3d exposing (Frame3d)
import Geometry.Types as Types
import Plane3d exposing (Plane3d)
import Point3d exposing (Point3d)
import QuadraticSpline2d exposing (QuadraticSpline2d)
import SketchPlane3d exposing (SketchPlane3d)
import Vector3d exposing (Vector3d)


{-| -}
type alias QuadraticSpline3d =
    Types.QuadraticSpline3d


{-| Construct a spline from its start point, control point and end point:

    exampleSpline =
        QuadraticSpline3d.with
            { startPoint =
                Point3d.fromCoordinates ( 1, 1, 1 )
            , controlPoint =
                Point3d.fromCoordinates ( 3, 2, 1 )
            , endPoint =
                Point3d.fromCoordinates ( 3, 3, 3 )
            }

-}
with : { startPoint : Point3d, controlPoint : Point3d, endPoint : Point3d } -> QuadraticSpline3d
with =
    Types.QuadraticSpline3d


{-| Construct a 3D spline lying _on_ a sketch plane by providing a 2D spline
specified in XY coordinates _within_ the sketch plane.

    QuadraticSpline3d.on SketchPlane3d.xz <|
        QuadraticSpline2d.with
            { startPoint =
                Point2d.fromCoordinates ( 1, 1 )
            , controlPoint =
                Point2d.fromCoordinates ( 3, 4 )
            , endPoint =
                Point2d.fromCoordinates ( 5, 1 )
            }
    --> QuadraticSpline3d.with
    -->     { startPoint =
    -->         Point3d.fromCoordinates ( 1, 0, 1 )
    -->     , controlPoint =
    -->         Point3d.fromCoordinates ( 3, 0, 4 )
    -->     , endPoint =
    -->         Point3d.fromCoordinates ( 5, 0, 1 )
    -->     }

-}
on : SketchPlane3d -> QuadraticSpline2d -> QuadraticSpline3d
on sketchPlane spline2d =
    with
        { startPoint =
            Point3d.on sketchPlane (QuadraticSpline2d.startPoint spline2d)
        , controlPoint =
            Point3d.on sketchPlane (QuadraticSpline2d.controlPoint spline2d)
        , endPoint =
            Point3d.on sketchPlane (QuadraticSpline2d.endPoint spline2d)
        }


{-| Get the start point of a spline.

    QuadraticSpline3d.startPoint exampleSpline
    --> Point3d.fromCoordinates ( 1, 1, 1 )

-}
startPoint : QuadraticSpline3d -> Point3d
startPoint (Types.QuadraticSpline3d spline) =
    spline.startPoint


{-| Get the end point of a spline. This is equal to the spline's last control
point.

    QuadraticSpline3d.endPoint exampleSpline
    --> Point3d.fromCoordinates ( 3, 3, 3 )

-}
endPoint : QuadraticSpline3d -> Point3d
endPoint (Types.QuadraticSpline3d spline) =
    spline.endPoint


{-| Get the control point of a spline.

    QuadraticSpline3d.controlPoint exampleSpline
    --> Point3d.fromCoordinates ( 3, 2, 1 )

-}
controlPoint : QuadraticSpline3d -> Point3d
controlPoint (Types.QuadraticSpline3d spline) =
    spline.controlPoint


{-| Get the start derivative of a spline. This is equal to twice the vector from
the spline's first control point to its second.

    QuadraticSpline3d.startDerivative exampleSpline
    --> Vector3d.fromComponents ( 4, 2, 0 )

-}
startDerivative : QuadraticSpline3d -> Vector3d
startDerivative spline =
    Vector3d.from (startPoint spline) (controlPoint spline)
        |> Vector3d.scaleBy 2


{-| Get the end derivative of a spline. This is equal to twice the vector from
the spline's second control point to its third.

    QuadraticSpline3d.endDerivative exampleSpline
    --> Vector3d.fromComponents ( 0, 2, 4 )

-}
endDerivative : QuadraticSpline3d -> Vector3d
endDerivative spline =
    Vector3d.from (controlPoint spline) (endPoint spline)
        |> Vector3d.scaleBy 2


{-| Compute a bounding box for a given spline. It is not guaranteed that the
result will be the _smallest_ possible bounding box, since for efficiency the
bounding box is computed from the spline's control points (which cover a larger
volume than the spline itself).

    QuadraticSpline3d.boundingBox exampleSpline
    --> BoundingBox3d.fromExtrema
    -->     { minX = 1
    -->     , maxX = 3
    -->     , minY = 1
    -->     , maxY = 3
    -->     , minZ = 1
    -->     , maxZ = 3
    -->     }

-}
boundingBox : QuadraticSpline3d -> BoundingBox3d
boundingBox spline =
    let
        ( x1, y1, z1 ) =
            Point3d.coordinates (startPoint spline)

        ( x2, y2, z2 ) =
            Point3d.coordinates (controlPoint spline)

        ( x3, y3, z3 ) =
            Point3d.coordinates (endPoint spline)
    in
    BoundingBox3d.fromExtrema
        { minX = min x1 (min x2 x3)
        , maxX = max x1 (max x2 x3)
        , minY = min y1 (min y2 y3)
        , maxY = max y1 (max y2 y3)
        , minZ = min z1 (min z2 z3)
        , maxZ = max z1 (max z2 z3)
        }


{-| Get the point along a spline at a given parameter value:

    QuadraticSpline3d.pointOn exampleSpline 0
    --> Point3d.fromCoordinates ( 1, 1, 1 )

    QuadraticSpline3d.pointOn exampleSpline 0.5
    --> Point3d.fromCoordinates ( 2.5, 2, 1.5 )

    QuadraticSpline3d.pointOn exampleSpline 1
    --> Point3d.fromCoordinates ( 3, 3, 3 )

-}
pointOn : QuadraticSpline3d -> ParameterValue -> Point3d
pointOn spline parameterValue =
    let
        t =
            ParameterValue.value parameterValue

        p1 =
            startPoint spline

        p2 =
            controlPoint spline

        p3 =
            endPoint spline

        q1 =
            Point3d.interpolateFrom p1 p2 t

        q2 =
            Point3d.interpolateFrom p2 p3 t
    in
    Point3d.interpolateFrom q1 q2 t


{-| Get points along a spline at a given set of parameter values:

    exampleSpline
        |> QuadraticSpline3d.pointsAt
            (ParameterValue.steps 2)
    --> [ Point3d.fromCoordinates ( 1, 1, 1 )
    --> , Point3d.fromCoordinates ( 2.5, 2, 1.5 )
    --> , Point3d.fromCoordinates ( 3, 3, 3 )
    --> ]

-}
pointsAt : List ParameterValue -> QuadraticSpline3d -> List Point3d
pointsAt parameterValues spline =
    List.map (pointOn spline) parameterValues


{-| Get the first derivative of a spline at a given parameter value:

    QuadraticSpline3d.derivative exampleSpline
        ParameterValue.zero
    --> Vector3d.fromComponents ( 4, 2, 0 )

    QuadraticSpline3d.derivative exampleSpline
        ParameterValue.half
    --> Vector3d.fromComponents ( 2, 2, 2 )

    QuadraticSpline3d.derivative exampleSpline
        ParameterValue.one
    --> Vector3d.fromComponents ( 0, 2, 4 )

Note that the derivative interpolates linearly from end to end.

-}
firstDerivative : QuadraticSpline3d -> ParameterValue -> Vector3d
firstDerivative spline parameterValue =
    let
        t =
            ParameterValue.value parameterValue

        p1 =
            startPoint spline

        p2 =
            controlPoint spline

        p3 =
            endPoint spline

        v1 =
            Vector3d.from p1 p2

        v2 =
            Vector3d.from p2 p3
    in
    Vector3d.interpolateFrom v1 v2 t |> Vector3d.scaleBy 2


{-| Evaluate the first derivative of a spline at a range of parameter values:

    exampleSpline
        |> QuadraticSpline3d.firstDerivativesAt
            (ParameterValue.steps 2)
    --> [ Vector3d.fromComponents ( 4, 2, 0 )
    --> , Vector3d.fromComponents ( 2, 2, 2 )
    --> , Vector3d.fromComponents ( 0, 2, 4 )
    --> ]

-}
firstDerivativesAt : List ParameterValue -> QuadraticSpline3d -> List Vector3d
firstDerivativesAt parameterValues spline =
    List.map (firstDerivative spline) parameterValues


derivativeMagnitude : QuadraticSpline3d -> ParameterValue -> Float
derivativeMagnitude spline =
    let
        ( x1, y1, z1 ) =
            Point3d.coordinates (startPoint spline)

        ( x2, y2, z2 ) =
            Point3d.coordinates (controlPoint spline)

        ( x3, y3, z3 ) =
            Point3d.coordinates (endPoint spline)

        x12 =
            x2 - x1

        y12 =
            y2 - y1

        z12 =
            z2 - z1

        x23 =
            x3 - x2

        y23 =
            y3 - y2

        z23 =
            z3 - z2

        x123 =
            x23 - x12

        y123 =
            y23 - y12

        z123 =
            z23 - z12
    in
    \parameterValue ->
        let
            t =
                ParameterValue.value parameterValue

            x13 =
                x12 + t * x123

            y13 =
                y12 + t * y123

            z13 =
                z12 + t * z123
        in
        2 * sqrt (x13 * x13 + y13 * y13 + z13 * z13)


{-| If a curve has zero length (consists of just a single point), then we say
that it is 'degenerate'. Some operations such as computing tangent directions
are not defined on degenerate curves.

A `Nondegenerate` value represents a spline that is definitely not degenerate.
It is used as input to functions such as `QuadraticSpline3d.tangentDirection`
and can be constructed using `QuadraticSpline3d.nondegenerate`.

-}
type Nondegenerate
    = NonZeroSecondDerivative QuadraticSpline3d Direction3d
    | NonZeroFirstDerivative QuadraticSpline3d Direction3d


{-| Attempt to construct a nondegenerate spline from a general
`QuadraticSpline3d`. If the spline is in fact degenerate (consists of a single
point), returns an `Err` with that point.

    QuadraticSpline3d.nondegenerate exampleSpline
    --> Ok nondegenerateExampleSpline

-}
nondegenerate : QuadraticSpline3d -> Result Point3d Nondegenerate
nondegenerate spline =
    case Vector3d.direction (secondDerivative spline) of
        Just direction ->
            Ok (NonZeroSecondDerivative spline direction)

        Nothing ->
            let
                -- Second derivative is zero, so first derivative is constant -
                -- evaluate it at an arbitrary point to get its value
                firstDerivativeVector =
                    firstDerivative spline ParameterValue.zero
            in
            case Vector3d.direction firstDerivativeVector of
                Just direction ->
                    Ok (NonZeroFirstDerivative spline direction)

                Nothing ->
                    Err (startPoint spline)


{-| Convert a nondegenerate spline back to a general `QuadraticSpline3d`.

    QuadraticSpline3d.fromNondegenerate
        nondegenerateExampleSpline
    --> exampleSpline

-}
fromNondegenerate : Nondegenerate -> QuadraticSpline3d
fromNondegenerate nondegenerateSpline =
    case nondegenerateSpline of
        NonZeroSecondDerivative spline _ ->
            spline

        NonZeroFirstDerivative spline _ ->
            spline


{-| Get the tangent direction to a nondegenerate spline at a given parameter
value:

    QuadraticSpline3d.tangentDirection
        nondegenerateExampleSpline
        ParameterValue.zero
    --> Direction3d.fromAzimuthAndElevation
    -->     (degrees 26.57)
    -->     (degrees 0)

    QuadraticSpline3d.tangentDirection
        nondegenerateExampleSpline
        ParameterValue.half
    --> Direction3d.fromAzimuthAndElevation
    -->     (degrees 45)
    -->     (degrees 35.26)

    QuadraticSpline3d.tangentDirection
        nondegenerateExampleSpline
        ParameterValue.one
    --> Direction3d.fromAzimuthAndElevation
    -->     (degrees 90)
    -->     (degrees 63.43)

-}
tangentDirection : Nondegenerate -> ParameterValue -> Direction3d
tangentDirection nondegenerateSpline parameterValue =
    case nondegenerateSpline of
        NonZeroSecondDerivative spline secondDerivativeDirection ->
            let
                firstDerivativeVector =
                    firstDerivative spline parameterValue
            in
            case Vector3d.direction firstDerivativeVector of
                Just firstDerivativeDirection ->
                    -- First derivative is non-zero, so use its direction as the
                    -- tangent direction
                    firstDerivativeDirection

                Nothing ->
                    -- Zero first derivative and non-zero second derivative mean
                    -- we have reached a reversal point, where the tangent
                    -- direction just afterwards is equal to the second
                    -- derivative direction and the tangent direction just
                    -- before is equal to the reversed second derivative
                    -- direction. If we happen to be right at the end of the
                    -- spline, choose the tangent direction just before the end
                    -- (instead of one that is off the spline!), otherwise
                    -- choose the tangent direction just after the point
                    -- (necessary for t = 0, arbitrary for all other points).
                    if parameterValue == ParameterValue.one then
                        Direction3d.reverse secondDerivativeDirection
                    else
                        secondDerivativeDirection

        NonZeroFirstDerivative spline firstDerivativeDirection ->
            -- Tangent direction is always equal to the (constant) first
            -- derivative direction
            firstDerivativeDirection


{-| Get tangent directions to a nondegenerate spline at a given set of parameter
values:

    nondegenerateExampleSpline
        |> QuadraticSpline3d.tangentDirectionsAt
            (ParameterValue.steps 2)
    --> [ Direction3d.fromAzimuthAndElevation
    -->     (degrees 26.57)
    -->     (degrees 0)
    --> , Direction3d.fromAzimuthAndElevation
    -->     (degrees 45)
    -->     (degrees 35.26)
    --> , Direction3d.fromAzimuthAndElevation
    -->     (degrees 90)
    -->     (degrees 63.43)
    --> ]

-}
tangentDirectionsAt : List ParameterValue -> Nondegenerate -> List Direction3d
tangentDirectionsAt parameterValues nondegenerateSpline =
    List.map (tangentDirection nondegenerateSpline) parameterValues


{-| Get both the point and tangent direction of a nondegenerate spline at a
given parameter value:

    QuadraticSpline3d.sample nondegenerateExampleSpline
        ParameterValue.half
    --> ( Point3d.fromCoordinates ( 2.5, 2, 1.5 )
    --> , Direction3d.fromAzimuthAndElevation
    -->     (degrees 45)
    -->     (degrees 35.26)
    --> )

-}
sample : Nondegenerate -> ParameterValue -> ( Point3d, Direction3d )
sample nondegenerateSpline parameterValue =
    ( pointOn (fromNondegenerate nondegenerateSpline) parameterValue
    , tangentDirection nondegenerateSpline parameterValue
    )


{-| Get points and tangent directions of a nondegenerate spline at a given set
of parameter values:

    nondegenerateExampleSpline
        |> QuadraticSpline3d.samplesAt
            (ParameterValue.steps 2)
    --> [ ( Point3d.fromCoordinates ( 1, 1, 1 )
    -->   , Direction3d.fromAzimuthAndElevation
    -->         (degrees 26.57)
    -->         (degrees 0)
    -->   )
    --> , ( Point3d.fromCoordinates ( 2.5, 2, 1.5 )
    -->   , Direction3d.fromAzimuthAndElevation
    -->         (degrees 45)
    -->         (degrees 35.26)
    -->   )
    --> , ( Point3d.fromCoordinates ( 3, 3, 3 )
    -->   , Direction3d.fromAzimuthAndElevation
    -->         (degrees 90)
    -->         (degrees 63.43)
    -->   )
    --> ]

-}
samplesAt : List ParameterValue -> Nondegenerate -> List ( Point3d, Direction3d )
samplesAt parameterValues nondegenerateSpline =
    List.map (sample nondegenerateSpline) parameterValues


{-| Reverse a spline so that the start point becomes the end point, and vice
versa.

    QuadraticSpline3d.reverse exampleSpline
    --> QuadraticSpline3d.with
    -->     { startPoint =
    -->         Point3d.fromCoordinates ( 3, 3, 3 )
    -->     , controlPoint =
    -->         Point3d.fromCoordinates ( 3, 2, 1 )
    -->     , endPoint =
    -->         Point3d.fromCoordinates ( 1, 1, 1 )
    -->     }

-}
reverse : QuadraticSpline3d -> QuadraticSpline3d
reverse spline =
    with
        { startPoint = endPoint spline
        , controlPoint = controlPoint spline
        , endPoint = startPoint spline
        }


{-| Scale a spline about the given center point by the given scale.

    exampleSpline
        |> QuadraticSpline3d.scaleAbout Point3d.origin 2
    --> QuadraticSpline3d.with
    -->     { startPoint =
    -->         Point3d.fromCoordinates ( 2, 2, 2 )
    -->     , controlPoint =
    -->         Point3d.fromCoordinates ( 6, 4, 2 )
    -->     , endPoint =
    -->         Point3d.fromCoordinates ( 6, 6, 6 )
    -->     }

-}
scaleAbout : Point3d -> Float -> QuadraticSpline3d -> QuadraticSpline3d
scaleAbout point scale =
    mapControlPoints (Point3d.scaleAbout point scale)


{-| Rotate a spline counterclockwise around a given axis by a given angle (in
radians).

    exampleSpline
        |> QuadraticSpline3d.rotateAround Axis3d.z
            (degrees 90)
    --> QuadraticSpline3d.with
    -->     { startPoint =
    -->         Point3d.fromCoordinates ( -1, 1, 1 )
    -->     , controlPoint =
    -->         Point3d.fromCoordinates ( -2, 3, 1 )
    -->     , endPoint =
    -->         Point3d.fromCoordinates ( -3, 3, 3 )
    -->     }

-}
rotateAround : Axis3d -> Float -> QuadraticSpline3d -> QuadraticSpline3d
rotateAround axis angle =
    mapControlPoints (Point3d.rotateAround axis angle)


{-| Translate a spline by a given displacement.

    displacement =
        Vector3d.fromComponents ( 2, 3, 1 )

    exampleSpline
        |> QuadraticSpline3d.translateBy displacement
    --> QuadraticSpline3d.with
    -->     { startPoint =
    -->         Point3d.fromCoordinates ( 3, 4, 2 )
    -->     , controlPoint =
    -->         Point3d.fromCoordinates ( 5, 5, 2 )
    -->     , endPoint =
    -->         Point3d.fromCoordinates ( 5, 6, 4 )
    -->     }

-}
translateBy : Vector3d -> QuadraticSpline3d -> QuadraticSpline3d
translateBy displacement =
    mapControlPoints (Point3d.translateBy displacement)


{-| Translate an arc in a given direction by a given distance;

    QuadraticSpline3d.translateIn direction distance

is equivalent to

    QuadraticSpline3d.translateBy
        (Vector3d.withLength distance direction)

-}
translateIn : Direction3d -> Float -> QuadraticSpline3d -> QuadraticSpline3d
translateIn direction distance spline =
    translateBy (Vector3d.withLength distance direction) spline


{-| Mirror a spline across a plane.

    QuadraticSpline3d.mirrorAcross Plane3d.xy exampleSpline
    --> QuadraticSpline3d.with
    -->     { startPoint =
    -->         Point3d.fromCoordinates ( 1, 1, -1 )
    -->     , controlPoint =
    -->         Point3d.fromCoordinates ( 3, 2, -1 )
    -->     , endPoint =
    -->         Point3d.fromCoordinates ( 3, 3, -3 )
    -->     }

-}
mirrorAcross : Plane3d -> QuadraticSpline3d -> QuadraticSpline3d
mirrorAcross plane =
    mapControlPoints (Point3d.mirrorAcross plane)


{-| Find the [orthographic projection](https://en.wikipedia.org/wiki/Orthographic_projection)
of a spline onto a plane.

    QuadraticSpline3d.projectOnto Plane3d.xy exampleSpline
    --> QuadraticSpline3d.with
    -->     { startPoint =
    -->         Point3d.fromCoordinates ( 1, 1, 0 )
    -->     , controlPoint =
    -->         Point3d.fromCoordinates ( 3, 2, 0 )
    -->     , endPoint =
    -->         Point3d.fromCoordinates ( 3, 3, 0 )
    -->     }

-}
projectOnto : Plane3d -> QuadraticSpline3d -> QuadraticSpline3d
projectOnto plane =
    mapControlPoints (Point3d.projectOnto plane)


{-| Take a spline defined in global coordinates, and return it expressed in
local coordinates relative to a given reference frame.

    localFrame =
        Frame3d.atPoint
            (Point3d.fromCoordinates ( 1, 2, 3 ))

    QuadraticSpline3d.relativeTo localFrame exampleSpline
    --> QuadraticSpline3d.with
    -->     { startPoint =
    -->         Point3d.fromCoordinates ( 0, -1, -2 )
    -->     , controlPoint =
    -->         Point3d.fromCoordinates ( 2, 0, -2 )
    -->     , endPoint =
    -->         Point3d.fromCoordinates ( 2, 1, 0 )
    -->     }

-}
relativeTo : Frame3d -> QuadraticSpline3d -> QuadraticSpline3d
relativeTo frame =
    mapControlPoints (Point3d.relativeTo frame)


{-| Take a spline considered to be defined in local coordinates relative to a
given reference frame, and return that spline expressed in global coordinates.

    localFrame =
        Frame3d.atPoint
            (Point3d.fromCoordinates ( 1, 2, 3 ))

    QuadraticSpline3d.placeIn localFrame exampleSpline
    --> QuadraticSpline3d.with
    -->     { startPoint =
    -->         Point3d.fromCoordinates ( 2, 3, 4 )
    -->     , controlPoint =
    -->         Point3d.fromCoordinates ( 4, 4, 4 )
    -->     , endPoint =
    -->         Point3d.fromCoordinates ( 4, 5, 6 )
    -->     }

-}
placeIn : Frame3d -> QuadraticSpline3d -> QuadraticSpline3d
placeIn frame =
    mapControlPoints (Point3d.placeIn frame)


{-| Project a spline into a given sketch plane. Conceptually, finds the
[orthographic projection](https://en.wikipedia.org/wiki/Orthographic_projection)
of the spline onto the plane and then expresses the projected spline in 2D
sketch coordinates.

    exampleSpline
        |> QuadraticSpline3d.projectInto SketchPlane3d.yz
    --> QuadraticSpline2d.with
    -->     { startPoint =
    -->         Point2d.fromCoordinates ( 1, 1 )
    -->     , controlPoint =
    -->         Point2d.fromCoordinates ( 2, 1 )
    -->     , endPoint =
    -->         Point2d.fromCoordinates ( 3, 3 )
    -->     }

-}
projectInto : SketchPlane3d -> QuadraticSpline3d -> QuadraticSpline2d
projectInto sketchPlane spline =
    QuadraticSpline2d.with
        { startPoint = Point3d.projectInto sketchPlane (startPoint spline)
        , controlPoint = Point3d.projectInto sketchPlane (controlPoint spline)
        , endPoint = Point3d.projectInto sketchPlane (endPoint spline)
        }


mapControlPoints : (Point3d -> Point3d) -> QuadraticSpline3d -> QuadraticSpline3d
mapControlPoints function spline =
    with
        { startPoint = function (startPoint spline)
        , controlPoint = function (controlPoint spline)
        , endPoint = function (endPoint spline)
        }


{-| Split a spline into two roughly equal halves.

    QuadraticSpline3d.bisect exampleSpline
    --> ( QuadraticSpline3d.with
    -->     { startPoint =
    -->         Point3d.fromCoordinates ( 1, 1, 1 )
    -->     , controlPoint =
    -->         Point3d.fromCoordinates ( 2, 2.5 )
    -->     , endPoint =
    -->         Point3d.fromCoordinates ( 3, 2.5 )
    -->     }
    --> , QuadraticSpline3d.with
    -->     { startPoint =
    -->         Point3d.fromCoordinates ( 3, 2.5 )
    -->     , controlPoint =
    -->         Point3d.fromCoordinates ( 4, 2.5 )
    -->     , endPoint =
    -->         Point3d.fromCoordinates ( 3, 3, 3 )
    -->     }
    --> )

Equivalent to `QuadraticSpline3d.splitAt ParameterValue.half`.

-}
bisect : QuadraticSpline3d -> ( QuadraticSpline3d, QuadraticSpline3d )
bisect =
    splitAt ParameterValue.half


{-| Split a spline at a particular parameter value, resulting in two smaller
splines.

    parameterValue =
        ParameterValue.clamped 0.75

    QuadraticSpline3d.splitAt parameterValue exampleSpline
    --> ( QuadraticSpline3d.with
    -->     { startPoint =
    -->         Point3d.fromCoordinates ( 1, 1, 1 )
    -->     , controlPoint =
    -->         Point3d.fromCoordinates ( 2, 1.5, 1 )
    -->     , endPoint =
    -->         Point3d.fromCoordinates ( 2.5, 2, 1.5 )
    -->     }
    --> , QuadraticSpline3d.with
    -->     { startPoint =
    -->         Point3d.fromCoordinates ( 2.5, 2, 1.5 )
    -->     , controlPoint =
    -->         Point3d.fromCoordinates ( 3, 2.5, 2 )
    -->     , endPoint =
    -->         Point3d.fromCoordinates ( 3, 3, 3 )
    -->     }
    --> )

-}
splitAt : ParameterValue -> QuadraticSpline3d -> ( QuadraticSpline3d, QuadraticSpline3d )
splitAt parameterValue spline =
    let
        t =
            ParameterValue.value parameterValue

        p1 =
            startPoint spline

        p2 =
            controlPoint spline

        p3 =
            endPoint spline

        q1 =
            Point3d.interpolateFrom p1 p2 t

        q2 =
            Point3d.interpolateFrom p2 p3 t

        r =
            Point3d.interpolateFrom q1 q2 t
    in
    ( with { startPoint = p1, controlPoint = q1, endPoint = r }
    , with { startPoint = r, controlPoint = q2, endPoint = p3 }
    )


{-| A spline that has been parameterized by arc length.
-}
type ArcLengthParameterized
    = ArcLengthParameterized
        { underlyingSpline : QuadraticSpline3d
        , parameterization : ArcLengthParameterization
        , nondegenerateSpline : Maybe Nondegenerate
        }


{-| Build an arc length parameterization of the given spline, with a given
accuracy. Generally speaking, all operations on the resulting
`ArcLengthParameterized` value will be accurate to within the specified maximum
error.

    parameterizedSpline =
        exampleSpline
            |> QuadraticSpline3d.arcLengthParameterized
                { maxError = 1.0e-4 }

-}
arcLengthParameterized : { maxError : Float } -> QuadraticSpline3d -> ArcLengthParameterized
arcLengthParameterized { maxError } spline =
    let
        parameterization =
            ArcLengthParameterization.build
                { maxError = maxError
                , derivativeMagnitude = derivativeMagnitude spline
                , maxSecondDerivativeMagnitude =
                    Vector3d.length (secondDerivative spline)
                }
    in
    ArcLengthParameterized
        { underlyingSpline = spline
        , parameterization = parameterization
        , nondegenerateSpline = Result.toMaybe (nondegenerate spline)
        }


{-| Find the total arc length of a spline.

    QuadraticSpline3d.arcLength parameterizedSpline
    --> 3.8175

In this example, the result will be accurate to within `1.0e-4` since that was
the tolerance used when constructing `parameterizedSpline`.

-}
arcLength : ArcLengthParameterized -> Float
arcLength parameterizedSpline =
    arcLengthParameterization parameterizedSpline
        |> ArcLengthParameterization.totalArcLength


{-| Try to get the point along a spline at a given arc length. For example, to
get the point a quarter of the way along `exampleSpline`:

    QuadraticSpline3d.pointAlong parameterizedSpline
        (arcLength / 4)
    --> Just <|
    -->     Point3d.fromCoordinates
    -->         ( 1.8227, 1.4655, 1.1083 )

Note that this is not the same as evaulating at a parameter value of 1/4:

    QuadraticSpline3d.pointOn exampleSpline 0.25
    --> Point3d.fromCoordinates ( 1.875, 1.5, 1.125 )

If the given arc length is less than zero or greater than the arc length of the
spline, returns `Nothing`.

-}
pointAlong : ArcLengthParameterized -> Float -> Maybe Point3d
pointAlong (ArcLengthParameterized parameterized) distance =
    parameterized.parameterization
        |> ArcLengthParameterization.arcLengthToParameterValue distance
        |> Maybe.map (pointOn parameterized.underlyingSpline)


{-| Try to get the tangent direction along a spline at a given arc length. To
get the tangent direction a quarter of the way along `exampleSpline`:

    QuadraticSpline3d.tangentDirectionAlong
        parameterizedSpline
        (arcLength / 4)
    --> Just <|
    -->     Direction3d.fromAzimuthAndElevation
    -->         (degrees 33.09)
    -->         (degrees 14.26)

If the given arc length is less than zero or greater than the arc length of the
spline (or if the spline is degenerate), returns `Nothing`.

-}
tangentDirectionAlong : ArcLengthParameterized -> Float -> Maybe Direction3d
tangentDirectionAlong (ArcLengthParameterized parameterized) distance =
    case parameterized.nondegenerateSpline of
        Just nondegenerateSpline ->
            parameterized.parameterization
                |> ArcLengthParameterization.arcLengthToParameterValue distance
                |> Maybe.map (tangentDirection nondegenerateSpline)

        Nothing ->
            Nothing


{-| Try to get the point and tangent direction along a spline at a given arc
length. To get the point and tangent direction a quarter of the way along
`exampleSpline`:

    QuadraticSpline3d.sampleAlong parameterizedSpline
        (0.25 * arcLength)
    --> Just
    -->     ( Point3d.fromCoordinates
    -->         ( 1.8227, 1.4655, 1.1083 )
    -->     , Direction3d.fromAzimuthAndElevation
    -->         (degrees 33.09)
    -->         (degrees 14.26)
    -->     )

If the given arc length is less than zero or greater than the arc length of the
spline (or if the spline is degenerate), `Nothing` is returned.

-}
sampleAlong : ArcLengthParameterized -> Float -> Maybe ( Point3d, Direction3d )
sampleAlong (ArcLengthParameterized parameterized) distance =
    case parameterized.nondegenerateSpline of
        Just nondegenerateSpline ->
            parameterized.parameterization
                |> ArcLengthParameterization.arcLengthToParameterValue distance
                |> Maybe.map (sample nondegenerateSpline)

        Nothing ->
            Nothing


{-| -}
arcLengthParameterization : ArcLengthParameterized -> ArcLengthParameterization
arcLengthParameterization (ArcLengthParameterized parameterized) =
    parameterized.parameterization


{-| Get the original `QuadraticSpline3d` from which an `ArcLengthParameterized`
value was constructed.
-}
fromArcLengthParameterized : ArcLengthParameterized -> QuadraticSpline3d
fromArcLengthParameterized (ArcLengthParameterized parameterized) =
    parameterized.underlyingSpline


{-| Get the second derivative of a spline (for a quadratic spline, this is a
constant).
-}
secondDerivative : QuadraticSpline3d -> Vector3d
secondDerivative spline =
    let
        p1 =
            startPoint spline

        p2 =
            controlPoint spline

        p3 =
            endPoint spline

        v1 =
            Vector3d.from p1 p2

        v2 =
            Vector3d.from p2 p3
    in
    Vector3d.difference v2 v1 |> Vector3d.scaleBy 2
