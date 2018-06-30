module CubicSpline3d
    exposing
        ( ArcLengthParameterized
        , CubicSpline3d
        , Nondegenerate
        , arcLength
        , arcLengthParameterization
        , arcLengthParameterized
        , bisect
        , boundingBox
        , endControlPoint
        , endDerivative
        , endPoint
        , firstDerivative
        , firstDerivativesAt
        , fromArcLengthParameterized
        , fromEndpoints
        , fromNondegenerate
        , fromQuadraticSpline
        , maxSecondDerivativeMagnitude
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
        , secondDerivativesAt
        , splitAt
        , startControlPoint
        , startDerivative
        , startPoint
        , tangentDirection
        , tangentDirectionAlong
        , tangentDirectionsAt
        , thirdDerivative
        , translateBy
        , translateIn
        , with
        )

{-| <img src="https://ianmackenzie.github.io/elm-geometry/1.0.0/CubicSpline3d/icon.svg" alt="CubicSpline3d" width="160">

A `CubicSpline3d` is a cubic [Bézier curve](https://en.wikipedia.org/wiki/B%C3%A9zier_curve)
in 3D defined by a start point, end point and two control points. This module
contains functionality for

  - Constructing splines
  - Evaluating points and derivatives along a spline
  - Scaling, rotating, translating or mirroring a spline
  - Converting a spline between local and global coordinates in different
    reference frames

@docs CubicSpline3d


# Constructors

@docs with, fromEndpoints, on, fromQuadraticSpline


# Properties

@docs startPoint, endPoint, startControlPoint, endControlPoint, startDerivative, endDerivative, boundingBox


# Evaluation

@docs pointOn, pointsAt
@docs Nondegenerate, nondegenerate, fromNondegenerate
@docs tangentDirection, tangentDirectionsAt, sample, samplesAt


# Transformations

@docs reverse, scaleAbout, rotateAround, translateBy, translateIn, mirrorAcross, projectOnto


# Coordinate conversions

@docs relativeTo, placeIn, projectInto


# Subdivision

@docs bisect, splitAt


# Arc length parameterization

@docs ArcLengthParameterized, arcLengthParameterized, arcLength, pointAlong, tangentDirectionAlong, sampleAlong


## Low level

An `ArcLengthParameterized` value is a combination of an
[`ArcLengthParameterization`](Geometry-ArcLengthParameterization) and an
underlying `CubicSpline3d`. If you need to do something fancy, you can extract
these two values separately.

@docs arcLengthParameterization, fromArcLengthParameterized


# Differentiation

You are unlikely to need to use these functions directly, but they are useful if
you are writing low-level geometric algorithms. As with the other curve
evaluation functions, passing a parameter value outside the range 0 to 1 will
result in `Nothing`.

@docs firstDerivative, firstDerivativesAt, secondDerivative, secondDerivativesAt, thirdDerivative, maxSecondDerivativeMagnitude

-}

import Axis3d exposing (Axis3d)
import BoundingBox3d exposing (BoundingBox3d)
import CubicSpline2d exposing (CubicSpline2d)
import Curve.ArcLengthParameterization as ArcLengthParameterization exposing (ArcLengthParameterization)
import Curve.ParameterValue as ParameterValue exposing (ParameterValue)
import Direction3d exposing (Direction3d)
import Frame3d exposing (Frame3d)
import Geometry.Types as Types
import Plane3d exposing (Plane3d)
import Point3d exposing (Point3d)
import QuadraticSpline3d exposing (QuadraticSpline3d)
import SketchPlane3d exposing (SketchPlane3d)
import Vector3d exposing (Vector3d)


{-| -}
type alias CubicSpline3d =
    Types.CubicSpline3d


{-| Construct a spline from its four control points:

    exampleSpline =
        CubicSpline3d.with
            { startPoint =
                Point3d.fromCoordinates ( 1, 1, 1 )
            , startControlPoint =
                Point3d.fromCoordinates ( 3, 1, 1 )
            , endControlPoint =
                Point3d.fromCoordinates ( 3, 3, 1 )
            , endPoint =
                Point3d.fromCoordinates ( 3, 3, 3 )
            }

-}
with : { startPoint : Point3d, startControlPoint : Point3d, endControlPoint : Point3d, endPoint : Point3d } -> CubicSpline3d
with =
    Types.CubicSpline3d


{-| Construct a spline from a given start point with a given start derivative,
to a given end point with a given end derivative, like so:

![Cubic spline from endpoints](https://ianmackenzie.github.io/elm-geometry/1.0.0/CubicSpline2d/fromEndpoints.svg)

The spline is based on a parameter that ranges from 0 to 1; as a result, in most
cases the length of each derivative vector should be roughly equal to the length
of the resulting spline.

-}
fromEndpoints : { startPoint : Point3d, startDerivative : Vector3d, endPoint : Point3d, endDerivative : Vector3d } -> CubicSpline3d
fromEndpoints arguments =
    let
        startControlPoint_ =
            arguments.startPoint
                |> Point3d.translateBy
                    (Vector3d.scaleBy (1 / 3) arguments.startDerivative)

        endControlPoint_ =
            arguments.endPoint
                |> Point3d.translateBy
                    (Vector3d.scaleBy (-1 / 3) arguments.endDerivative)
    in
    with
        { startPoint = arguments.startPoint
        , startControlPoint = startControlPoint_
        , endControlPoint = endControlPoint_
        , endPoint = arguments.endPoint
        }


{-| Construct a 3D spline lying _on_ a sketch plane by providing a 2D spline
specified in XY coordinates _within_ the sketch plane.

    CubicSpline3d.on SketchPlane3d.xz <|
        CubicSpline2d.with
            { startPoint =
                Point2d.fromCoordinates ( 1, 1 )
            , startControlPoint =
                Point2d.fromCoordinates ( 3, 4 )
            , endControlPoint =
                Point2d.fromCoordinates ( 5, 1 )
            , endPoint =
                Point2d.fromCoordinates ( 7, 4 )
            }
    --> CubicSpline3d.with
    -->     { startPoint =
    -->         Point3d.fromCoordinates ( 1, 0, 1 )
    -->     , startControlPoint =
    -->         Point3d.fromCoordinates ( 3, 0, 4 )
    -->     , endControlPoint =
    -->         Point3d.fromCoordinates ( 5, 0, 1 )
    -->     , endPoint =
    -->         Point3d.fromCoordinates ( 7, 0, 4 )
    -->     }

-}
on : SketchPlane3d -> CubicSpline2d -> CubicSpline3d
on sketchPlane spline2d =
    with
        { startPoint =
            Point3d.on sketchPlane (CubicSpline2d.startPoint spline2d)
        , startControlPoint =
            Point3d.on sketchPlane (CubicSpline2d.startControlPoint spline2d)
        , endControlPoint =
            Point3d.on sketchPlane (CubicSpline2d.endControlPoint spline2d)
        , endPoint =
            Point3d.on sketchPlane (CubicSpline2d.endPoint spline2d)
        }


{-| Convert a quadratic spline into the equivalent cubic spline (every quadratic
spline can be represented exactly as a cubic spline).

    quadraticSpline =
        QuadraticSpline3d.with
            { startPoint =
                Point3d.fromCoordinates ( 0, 0, 0  )
            , controlPoint =
                Point3d.fromCoordinates ( 3, 0, 0 )
            , endPoint =
                Point3d.fromCoordinates ( 3, 3, 0 )
            }

    CubicSpline3d.fromQuadraticSpline quadraticSpline
    --> CubicSpline3d.with
    -->     { startPoint =
    -->         Point3d.fromCoordinates ( 0, 0, 0 )
    -->     , startControlPoint =
    -->         Point3d.fromCoordinates ( 2, 0, 0 )
    -->     , endControlPoint =
    -->         Point3d.fromCoordinates ( 3, 1, 0 )
    -->     , endPoint =
    -->         Point3d.fromCoordinates ( 3, 3, 0 )
    -->     )

-}
fromQuadraticSpline : QuadraticSpline3d -> CubicSpline3d
fromQuadraticSpline quadraticSpline =
    let
        startPoint_ =
            QuadraticSpline3d.startPoint quadraticSpline

        controlPoint_ =
            QuadraticSpline3d.controlPoint quadraticSpline

        endPoint_ =
            QuadraticSpline3d.endPoint quadraticSpline

        startControlPoint_ =
            Point3d.interpolateFrom startPoint_ controlPoint_ (2 / 3)

        endControlPoint_ =
            Point3d.interpolateFrom endPoint_ controlPoint_ (2 / 3)
    in
    with
        { startPoint = startPoint_
        , startControlPoint = startControlPoint_
        , endControlPoint = endControlPoint_
        , endPoint = endPoint_
        }


{-| Get the start point of a spline.

    CubicSpline3d.startPoint exampleSpline
    --> Point3d.fromCoordinates ( 1, 1, 1 )

-}
startPoint : CubicSpline3d -> Point3d
startPoint (Types.CubicSpline3d spline) =
    spline.startPoint


{-| Get the end point of a spline.

    CubicSpline3d.endPoint exampleSpline
    --> Point3d.fromCoordinates ( 3, 3, 3 )

-}
endPoint : CubicSpline3d -> Point3d
endPoint (Types.CubicSpline3d spline) =
    spline.endPoint


{-| Get the start control point of a spline (the control point next to the
start point).
-}
startControlPoint : CubicSpline3d -> Point3d
startControlPoint (Types.CubicSpline3d spline) =
    spline.startControlPoint


{-| Get the end control point of a spline (the control point next to the
end point).
-}
endControlPoint : CubicSpline3d -> Point3d
endControlPoint (Types.CubicSpline3d spline) =
    spline.endControlPoint


{-| Get the start derivative of a spline. This is equal to three times the
vector from the spline's start point to its start control point.

    CubicSpline3d.startDerivative exampleSpline
    --> Vector3d.fromComponents ( 6, 0, 0 )

-}
startDerivative : CubicSpline3d -> Vector3d
startDerivative spline =
    Vector3d.from (startPoint spline) (startControlPoint spline)
        |> Vector3d.scaleBy 3


{-| Get the end derivative of a spline. This is equal to three times the vector
from the spline's end control point to its end point.

    CubicSpline3d.endDerivative exampleSpline
    --> Vector3d.fromComponents ( 0, 0, 6 )

-}
endDerivative : CubicSpline3d -> Vector3d
endDerivative spline =
    Vector3d.from (endControlPoint spline) (endPoint spline)
        |> Vector3d.scaleBy 3


{-| Compute a bounding box for a given spline. It is not guaranteed that the
result will be the _smallest_ possible bounding box, since for efficiency the
bounding box is computed from the spline's control points (which cover a larger
volume than the spline itself).

    CubicSpline3d.boundingBox exampleSpline
    --> BoundingBox3d.fromExtrema
    -->     { minX = 1
    -->     , maxX = 3
    -->     , minY = 1
    -->     , maxY = 3
    -->     , minZ = 1
    -->     , maxZ = 3
    -->     }

-}
boundingBox : CubicSpline3d -> BoundingBox3d
boundingBox spline =
    let
        ( x1, y1, z1 ) =
            Point3d.coordinates (startPoint spline)

        ( x2, y2, z2 ) =
            Point3d.coordinates (startControlPoint spline)

        ( x3, y3, z3 ) =
            Point3d.coordinates (endControlPoint spline)

        ( x4, y4, z4 ) =
            Point3d.coordinates (endPoint spline)
    in
    BoundingBox3d.fromExtrema
        { minX = min (min x1 x2) (min x3 x4)
        , maxX = max (max x1 x2) (max x3 x4)
        , minY = min (min y1 y2) (min y3 y4)
        , maxY = max (max y1 y2) (max y3 y4)
        , minZ = min (min z1 z2) (min z3 z4)
        , maxZ = max (max z1 z2) (max z3 z4)
        }


{-| Get a point at a given parameter value.

    CubicSpline3d.pointOn exampleSpline ParameterValue.zero
    --> Point3d.fromCoordinates ( 1, 1, 1 )

    CubicSpline3d.pointOn exampleSpline ParameterValue.half
    --> Point3d.fromCoordinates ( 2.75, 2, 1.25 )

    CubicSpline3d.pointOn exampleSpline ParameterValue.one
    --> Point3d.fromCoordinates ( 3, 3, 3 )

-}
pointOn : CubicSpline3d -> ParameterValue -> Point3d
pointOn spline parameterValue =
    let
        t =
            ParameterValue.value parameterValue

        p1 =
            startPoint spline

        p2 =
            startControlPoint spline

        p3 =
            endControlPoint spline

        p4 =
            endPoint spline

        q1 =
            Point3d.interpolateFrom p1 p2 t

        q2 =
            Point3d.interpolateFrom p2 p3 t

        q3 =
            Point3d.interpolateFrom p3 p4 t

        r1 =
            Point3d.interpolateFrom q1 q2 t

        r2 =
            Point3d.interpolateFrom q2 q3 t
    in
    Point3d.interpolateFrom r1 r2 t


{-| Get points along a spline at a given set of parameter values.

    exampleSpline
        |> CubicSpline3d.pointsAt
            (ParameterValue.steps 2)
    --> [ Point3d.fromCoordinates ( 1, 1, 1 )
    --> , Point3d.fromCoordinates ( 2.75, 2, 1.25 )
    --> , Point3d.fromCoordinates ( 3, 3, 3 )
    --> ]

-}
pointsAt : List ParameterValue -> CubicSpline3d -> List Point3d
pointsAt parameterValues spline =
    List.map (pointOn spline) parameterValues


{-| If a curve has zero length (consists of just a single point), then we say
that it is 'degenerate'. Some operations such as computing tangent directions
are not defined on degenerate curves.

A `Nondegenerate` value represents a spline that is definitely not degenerate.
It is used as input to functions such as `CubicSpline3d.tangentDirection` and
can be constructed using `CubicSpline3d.nondegenerate`.

-}
type Nondegenerate
    = NonZeroThirdDerivative CubicSpline3d Direction3d
    | NonZeroSecondDerivative CubicSpline3d Direction3d
    | NonZeroFirstDerivative CubicSpline3d Direction3d


{-| Attempt to construct a nondegenerate spline from a general `CubicSpline3d`.
If the spline is in fact degenerate (consists of a single point), returns an
`Err` with that point.

    CubicSpline3d.nondegenerate exampleSpline
    --> Ok nondegenerateExampleSpline

-}
nondegenerate : CubicSpline3d -> Result Point3d Nondegenerate
nondegenerate spline =
    case Vector3d.direction (thirdDerivative spline) of
        Just direction ->
            -- Third derivative is non-zero, so if all else fails we can fall
            -- back on it to provide a tangent direction
            Ok (NonZeroThirdDerivative spline direction)

        Nothing ->
            let
                -- Third derivative is zero, so second derivative is constant -
                -- evaluate it at an arbitrary point to get its value
                secondDerivativeVector =
                    secondDerivative spline ParameterValue.zero
            in
            case Vector3d.direction secondDerivativeVector of
                Just direction ->
                    -- Second derivative is non-zero, so if all else fails we
                    -- can fall back on it to provide a tangent direction
                    Ok (NonZeroSecondDerivative spline direction)

                Nothing ->
                    let
                        -- Second and third derivatives are zero, so first
                        -- derivative is constant - evaluate it at an arbitrary
                        -- point to get its value
                        firstDerivativeVector =
                            firstDerivative spline ParameterValue.zero
                    in
                    case Vector3d.direction firstDerivativeVector of
                        Just direction ->
                            -- First derivative is constant and non-zero, so the
                            -- tangent direction will always be equal to the
                            -- first derivative direction
                            Ok (NonZeroFirstDerivative spline direction)

                        Nothing ->
                            Err (startPoint spline)


{-| Convert a nondegenerate spline back to a general `CubicSpline3d`.

    CubicSpline3d.fromNondegenerate
        nondegenerateExampleSpline
    --> exampleSpline

-}
fromNondegenerate : Nondegenerate -> CubicSpline3d
fromNondegenerate nondegenerateSpline =
    case nondegenerateSpline of
        NonZeroThirdDerivative spline _ ->
            spline

        NonZeroSecondDerivative spline _ ->
            spline

        NonZeroFirstDerivative spline _ ->
            spline


{-| Get the tangent direction to a nondegenerate spline at a given parameter
value:

    CubicSpline3d.tangentDirection
        nondegenerateExampleSpline
        ParameterValue.zero
    --> Direction3d.x

    CubicSpline3d.tangentDirection
        nondegenerateExampleSpline
        ParameterValue.half
    --> Direction3d.fromAzimuthAndElevation
    -->     (degrees 63.43)
    -->     (degrees 24.09)

    CubicSpline3d.tangentDirection
        nondegenerateExampleSpline
        ParameterValue.one
    --> Direction3d.z

-}
tangentDirection : Nondegenerate -> ParameterValue -> Direction3d
tangentDirection nondegenerateSpline parameterValue =
    case nondegenerateSpline of
        NonZeroFirstDerivative spline firstDerivativeDirection ->
            -- Tangent direction is always equal to the (constant) first
            -- derivative direction
            firstDerivativeDirection

        NonZeroSecondDerivative spline secondDerivativeDirection ->
            let
                firstDerivativeVector =
                    firstDerivative spline parameterValue
            in
            case Vector3d.direction firstDerivativeVector of
                Just firstDerivativeDirection ->
                    -- First derivative is non-zero, so use its direction as the
                    -- tangent direction (normal case)
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

        NonZeroThirdDerivative spline thirdDerivativeDirection ->
            let
                firstDerivativeVector =
                    firstDerivative spline parameterValue
            in
            case Vector3d.direction firstDerivativeVector of
                Just firstDerivativeDirection ->
                    -- First derivative is non-zero, so use its direction as the
                    -- tangent direction (normal case)
                    firstDerivativeDirection

                Nothing ->
                    let
                        secondDerivativeVector =
                            secondDerivative spline parameterValue
                    in
                    case Vector3d.direction secondDerivativeVector of
                        Just secondDerivativeDirection ->
                            -- Zero first derivative and non-zero second
                            -- derivative mean we have reached a reversal point,
                            -- as above in the NonZeroSecondDerivative case
                            if parameterValue == ParameterValue.one then
                                Direction3d.reverse secondDerivativeDirection
                            else
                                secondDerivativeDirection

                        Nothing ->
                            -- First and second derivatives are zero, so fall
                            -- back to the third derivative direction
                            thirdDerivativeDirection


{-| Get tangent directions to a nondegenerate spline at a given set of parameter
values:

    nondegenerateExampleSpline
        |> CubicSpline3d.tangentDirectionsAt
            (ParameterValue.steps 2)
    --> [ Direction3d.x
    --> , Direction3d.fromAzimuthAndElevation
    -->     (degrees 63.43)
    -->     (degrees 24.09)
    --> , Direction3d.z
    --> ]

-}
tangentDirectionsAt : List ParameterValue -> Nondegenerate -> List Direction3d
tangentDirectionsAt parameterValues nondegenerateSpline =
    List.map (tangentDirection nondegenerateSpline) parameterValues


{-| Get both the point and tangent direction of a nondegenerate spline at a
given parameter value:

    CubicSpline3d.sample nondegenerateExampleSpline
        ParameterValue.half
    --> ( Point3d.fromCoordinates ( 2.75, 2, 1.25 )
    --> , Direction3d.fromAzimuthAndElevation
    -->     (degrees 63.43)
    -->     (degrees 24.09)
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
        |> CubicSpline3d.samplesAt
            (ParameterValue.steps 2)
    --> [ ( Point3d.fromCoordinates ( 1, 1, 1 )
    -->   , Direction3d.x
    -->   )
    --> , ( Point3d.fromCoordinates ( 2.75, 2, 1.25 )
    -->   , Direction3d.fromAzimuthAndElevation
    -->         (degrees 63.43)
    -->         (degrees 24.09)
    -->   )
    --> , ( Point3d.fromCoordinates ( 3, 3, 3 )
    -->   , Direction3d.z
    -->   )
    --> ]

-}
samplesAt : List ParameterValue -> Nondegenerate -> List ( Point3d, Direction3d )
samplesAt parameterValues nondegenerateSpline =
    List.map (sample nondegenerateSpline) parameterValues


{-| Reverse a spline so that the start point becomes the end point, and vice
versa.

    CubicSpline3d.reverse exampleSpline
    --> CubicSpline3d.with
    -->     { startPoint =
    -->         Point3d.fromCoordinates ( 3, 3, 3 )
    -->     , startControlPoint =
    -->         Point3d.fromCoordinates ( 3, 3, 1 )
    -->     , endControlPoint =
    -->         Point3d.fromCoordinates ( 3, 1, 1 )
    -->     , endPoint =
    -->         Point3d.fromCoordinates ( 1, 1, 1 )
    -->     }

-}
reverse : CubicSpline3d -> CubicSpline3d
reverse spline =
    with
        { startPoint = endPoint spline
        , startControlPoint = endControlPoint spline
        , endControlPoint = startControlPoint spline
        , endPoint = startPoint spline
        }


{-| Scale a spline about the given center point by the given scale.

    CubicSpline3d.scaleAbout Point3d.origin 2 exampleSpline
    --> CubicSpline3d.with
    -->     { startPoint =
    -->         Point3d.fromCoordinates ( 2, 2, 2 )
    -->     , startControlPoint =
    -->         Point3d.fromCoordinates ( 6, 2, 2 )
    -->     , endControlPoint =
    -->         Point3d.fromCoordinates ( 6, 6, 2 )
    -->     , endPoint =
    -->         Point3d.fromCoordinates ( 6, 6, 6 )
    -->     }

-}
scaleAbout : Point3d -> Float -> CubicSpline3d -> CubicSpline3d
scaleAbout point scale =
    mapControlPoints (Point3d.scaleAbout point scale)


{-| Rotate a spline counterclockwise around a given axis by a given angle (in
radians).

    exampleSpline
        |> CubicSpline3d.rotateAround Axis3d.z (degrees 90)
    --> CubicSpline3d.with
    -->     { startPoint =
    -->         Point3d.fromCoordinates ( -1, 1, 1 )
    -->     , startControlPoint =
    -->         Point3d.fromCoordinates ( -1, 3, 1 )
    -->     , endControlPoint =
    -->         Point3d.fromCoordinates ( -3, 3, 1 )
    -->     , endPoint =
    -->         Point3d.fromCoordinates ( -3, 3, 3 )
    -->     }

-}
rotateAround : Axis3d -> Float -> CubicSpline3d -> CubicSpline3d
rotateAround axis angle =
    mapControlPoints (Point3d.rotateAround axis angle)


{-| Translate a spline by a given displacement.

    displacement =
        Vector3d.fromComponents ( 2, 3, 1 )

    CubicSpline3d.translateBy displacement exampleSpline
    --> CubicSpline3d.with
    -->     { startPoint =
    -->         Point3d.fromCoordinates ( 3, 4, 2 )
    -->     , startControlPoint =
    -->         Point3d.fromCoordinates ( 5, 4, 2 )
    -->     , endControlPoint =
    -->         Point3d.fromCoordinates ( 5, 6, 2 )
    -->     , endPoint =
    -->         Point3d.fromCoordinates ( 5, 6, 4 )
    -->     }

-}
translateBy : Vector3d -> CubicSpline3d -> CubicSpline3d
translateBy displacement =
    mapControlPoints (Point3d.translateBy displacement)


{-| Translate a spline in a given direction by a given distance;

    CubicSpline3d.translateIn direction distance

is equivalent to

    CubicSpline3d.translateBy
        (Vector3d.withLength distance direction)

-}
translateIn : Direction3d -> Float -> CubicSpline3d -> CubicSpline3d
translateIn direction distance spline =
    translateBy (Vector3d.withLength distance direction) spline


{-| Mirror a spline across a plane.

    CubicSpline3d.mirrorAcross Plane3d.xy exampleSpline
    --> CubicSpline3d.with
    -->     { startPoint =
    -->         Point3d.fromCoordinates ( 1, 1, -1 )
    -->     , startControlPoint =
    -->         Point3d.fromCoordinates ( 3, 1, -1 )
    -->     , endControlPoint =
    -->         Point3d.fromCoordinates ( 3, 3, -1 )
    -->     , endPoint =
    -->         Point3d.fromCoordinates ( 3, 3, -3 )
    -->     }

-}
mirrorAcross : Plane3d -> CubicSpline3d -> CubicSpline3d
mirrorAcross plane =
    mapControlPoints (Point3d.mirrorAcross plane)


{-| Find the [orthographic projection](https://en.wikipedia.org/wiki/Orthographic_projection)
of a spline onto a plane.

    CubicSpline3d.projectOnto Plane3d.xy exampleSpline
    --> CubicSpline3d.with
    -->     { startPoint =
    -->         Point3d.fromCoordinates ( 1, 1, 0 )
    -->     , startControlPoint =
    -->         Point3d.fromCoordinates ( 3, 1, 0 )
    -->     , endControlPoint =
    -->         Point3d.fromCoordinates ( 3, 3, 0 )
    -->     , endPoint =
    -->         Point3d.fromCoordinates ( 3, 3, 0 )
    -->     }

-}
projectOnto : Plane3d -> CubicSpline3d -> CubicSpline3d
projectOnto plane =
    mapControlPoints (Point3d.projectOnto plane)


{-| Take a spline defined in global coordinates, and return it expressed in
local coordinates relative to a given reference frame.

    localFrame =
        Frame3d.atPoint
            (Point3d.fromCoordinates ( 1, 2, 3 ))

    CubicSpline3d.relativeTo localFrame exampleSpline
    --> CubicSpline3d.with
    -->     { startPoint =
    -->         Point3d.fromCoordinates ( 0, -1, -2 )
    -->     , startControlPoint =
    -->         Point3d.fromCoordinates ( 2, -1, -2 )
    -->     , endControlPoint =
    -->         Point3d.fromCoordinates ( 2, 1, -2 )
    -->     , endPoint =
    -->         Point3d.fromCoordinates ( 2, 1, 0 )
    -->     }

-}
relativeTo : Frame3d -> CubicSpline3d -> CubicSpline3d
relativeTo frame =
    mapControlPoints (Point3d.relativeTo frame)


{-| Take a spline considered to be defined in local coordinates relative to a
given reference frame, and return that spline expressed in global coordinates.

    localFrame =
        Frame3d.atPoint
            (Point3d.fromCoordinates ( 1, 2, 3 ))

    CubicSpline3d.placeIn localFrame exampleSpline
    --> CubicSpline3d.with
    -->     { startPoint =
    -->         Point3d.fromCoordinates ( 2, 3, 4 )
    -->     , startControlPoint =
    -->         Point3d.fromCoordinates ( 4, 3, 4 )
    -->     , endControlPoint =
    -->         Point3d.fromCoordinates ( 4, 5, 4 )
    -->     , endPoint =
    -->         Point3d.fromCoordinates ( 4, 5, 6 )
    -->     }

-}
placeIn : Frame3d -> CubicSpline3d -> CubicSpline3d
placeIn frame =
    mapControlPoints (Point3d.placeIn frame)


{-| Project a spline into a given sketch plane. Conceptually, this finds the
[orthographic projection](https://en.wikipedia.org/wiki/Orthographic_projection)
of the spline onto the plane and then expresses the projected spline in 2D
sketch coordinates.

    exampleSpline
        |> CubicSpline3d.projectInto SketchPlane3d.yz
    --> CubicSpline2d.with
    -->     { startPoint =
    -->         Point2d.fromCoordinates ( 1, 1 )
    -->     , startControlPoint =
    -->         Point2d.fromCoordinates ( 1, 1 )
    -->     , endControlPoint =
    -->         Point2d.fromCoordinates ( 3, 1 )
    -->     , endPoint =
    -->         Point2d.fromCoordinates ( 3, 3 )
    -->     }

-}
projectInto : SketchPlane3d -> CubicSpline3d -> CubicSpline2d
projectInto sketchPlane spline =
    CubicSpline2d.with
        { startPoint =
            Point3d.projectInto sketchPlane (startPoint spline)
        , startControlPoint =
            Point3d.projectInto sketchPlane (startControlPoint spline)
        , endControlPoint =
            Point3d.projectInto sketchPlane (endControlPoint spline)
        , endPoint =
            Point3d.projectInto sketchPlane (endPoint spline)
        }


mapControlPoints : (Point3d -> Point3d) -> CubicSpline3d -> CubicSpline3d
mapControlPoints function spline =
    with
        { startPoint = function (startPoint spline)
        , startControlPoint = function (startControlPoint spline)
        , endControlPoint = function (endControlPoint spline)
        , endPoint = function (endPoint spline)
        }


{-| Split a spline into two roughly equal halves.

    CubicSpline3d.bisect exampleSpline
    --> ( CubicSpline3d.with
    -->     { startPoint =
    -->         Point3d.fromCoordinates ( 1, 1, 1 )
    -->     , startControlPoint =
    -->         Point3d.fromCoordinates ( 2, 1, 1 )
    -->     , endControlPoint =
    -->         Point3d.fromCoordinates ( 2.5, 1.5, 1 )
    -->     , endPoint =
    -->         Point3d.fromCoordinates ( 2.75, 2, 1.25 )
    -->     }
    --> , CubicSpline3d.with
    -->     { startPoint =
    -->         Point3d.fromCoordinates ( 2.75, 2, 1.25 )
    -->     , startControlPoint =
    -->         Point3d.fromCoordinates ( 3, 2.5, 1.5 )
    -->     , endControlPoint =
    -->         Point3d.fromCoordinates ( 3, 3, 2 )
    -->     , endPoint =
    -->         Point3d.fromCoordinates ( 3, 3, 3 )
    -->     }
    --> )

Equivalent to `CubicSpline3d.splitAt ParameterValue.half`.

-}
bisect : CubicSpline3d -> ( CubicSpline3d, CubicSpline3d )
bisect =
    splitAt ParameterValue.half


{-| Split a spline at a particular parameter value, resulting in two smaller
splines.

    parameterValue =
        ParameterValue.clamped 0.75

    CubicSpline3d.splitAt parameterValue exampleSpline
    --> ( CubicSpline3d.with
    -->     { startPoint =
    -->         Point3d.fromCoordinates ( 1, 1, 1 )
    -->     , startControlPoint =
    -->         Point3d.fromCoordinates ( 2.5, 1, 1 )
    -->     , endControlPoint =
    -->         Point3d.fromCoordinates ( 2.88, 2.13, 1 )
    -->     , endPoint =
    -->         Point3d.fromCoordinates ( 2.97, 2.69, 1.84 )
    -->     }
    --> , CubicSpline3d.with
    -->     { startPoint =
    -->         Point3d.fromCoordinates ( 2.97, 2.69, 1.84 )
    -->     , startControlPoint =
    -->         Point3d.fromCoordinates ( 3, 2.88, 2.13 )
    -->     , endControlPoint =
    -->         Point3d.fromCoordinates ( 3, 3, 2.5 )
    -->     , endPoint =
    -->         Point3d.fromCoordinates ( 3, 3, 3 )
    -->     }
    --> )

-}
splitAt : ParameterValue -> CubicSpline3d -> ( CubicSpline3d, CubicSpline3d )
splitAt parameterValue spline =
    let
        t =
            ParameterValue.value parameterValue

        p1 =
            startPoint spline

        p2 =
            startControlPoint spline

        p3 =
            endControlPoint spline

        p4 =
            endPoint spline

        q1 =
            Point3d.interpolateFrom p1 p2 t

        q2 =
            Point3d.interpolateFrom p2 p3 t

        q3 =
            Point3d.interpolateFrom p3 p4 t

        r1 =
            Point3d.interpolateFrom q1 q2 t

        r2 =
            Point3d.interpolateFrom q2 q3 t

        s =
            Point3d.interpolateFrom r1 r2 t
    in
    ( with
        { startPoint = p1
        , startControlPoint = q1
        , endControlPoint = r1
        , endPoint = s
        }
    , with
        { startPoint = s
        , startControlPoint = r2
        , endControlPoint = q3
        , endPoint = p4
        }
    )


{-| A spline that has been parameterized by arc length.
-}
type ArcLengthParameterized
    = ArcLengthParameterized
        { underlyingSpline : CubicSpline3d
        , parameterization : ArcLengthParameterization
        , nondegenerateSpline : Maybe Nondegenerate
        }


{-| Build an arc length parameterization of the given spline, with a given
accuracy. Generally speaking, all operations on the resulting
`ArcLengthParameterized` value will be accurate to within the specified maximum
error.

    parameterizedSpline =
        exampleSpline
            |> CubicSpline3d.arcLengthParameterized
                { maxError = 1.0e-4 }

-}
arcLengthParameterized : { maxError : Float } -> CubicSpline3d -> ArcLengthParameterized
arcLengthParameterized { maxError } spline =
    let
        parameterization =
            ArcLengthParameterization.build
                { maxError = maxError
                , derivativeMagnitude = derivativeMagnitude spline
                , maxSecondDerivativeMagnitude =
                    maxSecondDerivativeMagnitude spline
                }
    in
    ArcLengthParameterized
        { underlyingSpline = spline
        , parameterization = parameterization
        , nondegenerateSpline = Result.toMaybe (nondegenerate spline)
        }


{-| Find the total arc length of a spline:

    arcLength =
        CubicSpline3d.arcLength parameterizedSpline

    arcLength
    --> 4.3303

In this example, the result will be accurate to within `1.0e-4` since that was
the tolerance used when constructing `parameterizedSpline`.

-}
arcLength : ArcLengthParameterized -> Float
arcLength parameterizedSpline =
    arcLengthParameterization parameterizedSpline
        |> ArcLengthParameterization.totalArcLength


{-| Try to get the point along a spline at a given arc length. For example, to
get the point a quarter of the way along `exampleSpline`:

    CubicSpline3d.pointAlong parameterizedSpline
        (arcLength / 4)
    --> Just <|
    -->     Point3d.fromCoordinates
    -->         ( 2.0425, 1.2431, 1.0206 )

Note that this is not the same as evaulating at a parameter value of 1/4:

    CubicSpline3d.pointOn exampleSpline
        (ParameterValue.clamped 0.25)
    --> Point3d.fromCoordinates ( 2.1563, 1.3125, 1.0313 )

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

    CubicSpline3d.tangentDirectionAlong parameterizedSpline
        (0.25 * arcLength)
    --> Just
    -->     (Direction3d.fromAzimuthAndElevation
    -->         (degrees 29.1)
    -->         (degrees 3.871)
    -->     )

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

    CubicSpline3d.sampleAlong parameterizedSpline
        (0.25 * arcLength)
    --> Just
    -->     ( Point3d.fromCoordinates
    -->         ( 2.0425, 1.2431, 1.0206 )
    -->     , Direction3d.fromAzimuthAndElevation
    -->         (degrees 29.1)
    -->         (degrees 3.871)
    -->     )

If the given arc length is less than zero or greater than the arc length of the
spline (or if the spline is degenerate), returns `Nothing`.

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


{-| -}
fromArcLengthParameterized : ArcLengthParameterized -> CubicSpline3d
fromArcLengthParameterized (ArcLengthParameterized parameterized) =
    parameterized.underlyingSpline


{-| Get the first derivative of a spline at a given parameter value.

    CubicSpline3d.derivative exampleSpline
        ParameterValue.zero
    --> Vector3d.fromComponents ( 6, 0, 0 )

    CubicSpline3d.derivative exampleSpline
        ParameterValue.half
    --> Vector3d.fromComponents ( 1.5, 3, 1.5 )

    CubicSpline3d.derivative exampleSpline
        ParameterValue.one
    --> Vector3d.fromComponents ( 0, 0, 6 )

-}
firstDerivative : CubicSpline3d -> ParameterValue -> Vector3d
firstDerivative spline parameterValue =
    let
        t =
            ParameterValue.value parameterValue

        p1 =
            startPoint spline

        p2 =
            startControlPoint spline

        p3 =
            endControlPoint spline

        p4 =
            endPoint spline

        ( x1, y1, z1 ) =
            Point3d.coordinates p1

        ( x2, y2, z2 ) =
            Point3d.coordinates p2

        ( x3, y3, z3 ) =
            Point3d.coordinates p3

        ( x4, y4, z4 ) =
            Point3d.coordinates p4

        vx1 =
            x2 - x1

        vy1 =
            y2 - y1

        vz1 =
            z2 - z1

        vx2 =
            x3 - x2

        vy2 =
            y3 - y2

        vz2 =
            z3 - z2

        vx3 =
            x4 - x3

        vy3 =
            y4 - y3

        vz3 =
            z4 - z3
    in
    if t <= 0.5 then
        let
            wx1 =
                vx1 + t * (vx2 - vx1)

            wy1 =
                vy1 + t * (vy2 - vy1)

            wz1 =
                vz1 + t * (vz2 - vz1)

            wx2 =
                vx2 + t * (vx3 - vx2)

            wy2 =
                vy2 + t * (vy3 - vy2)

            wz2 =
                vz2 + t * (vz3 - vz2)
        in
        Vector3d.fromComponents
            ( 3 * (wx1 + t * (wx2 - wx1))
            , 3 * (wy1 + t * (wy2 - wy1))
            , 3 * (wz1 + t * (wz2 - wz1))
            )
    else
        let
            u =
                1 - t

            wx1 =
                vx2 + u * (vx1 - vx2)

            wy1 =
                vy2 + u * (vy1 - vy2)

            wz1 =
                vz2 + u * (vz1 - vz2)

            wx2 =
                vx3 + u * (vx2 - vx3)

            wy2 =
                vy3 + u * (vy2 - vy3)

            wz2 =
                vz3 + u * (vz2 - vz3)
        in
        Vector3d.fromComponents
            ( 3 * (wx2 + u * (wx1 - wx2))
            , 3 * (wy2 + u * (wy1 - wy2))
            , 3 * (wz2 + u * (wz1 - wz2))
            )


{-| Evaluate the first derivative of a spline at a range of parameter values.

    exampleSpline
        |> CubicSpline3d.firstDerivativesAt
            (ParameterValue.steps 2)
    --> [ Vector3d.fromComponents ( 6, 0, 0 )
    --> , Vector3d.fromComponents ( 1.5, 3, 1.5 )
    --> , Vector3d.fromComponents ( 0, 0, 6 )
    --> ]

-}
firstDerivativesAt : List ParameterValue -> CubicSpline3d -> List Vector3d
firstDerivativesAt parameterValues spline =
    List.map (firstDerivative spline) parameterValues


{-| Get the second derivative value at a point along a spline, based on a
parameter that ranges from 0 to 1. A parameter value of 0 corresponds to the
start of the spline and a value of 1 corresponds to the end.

    CubicSpline3d.secondDerivative exampleSpline
        ParameterValue.zero
    --> Vector3d.fromComponents ( -12, 12, 0 )

    CubicSpline3d.secondDerivative exampleSpline
        ParameterValue.half
    --> Vector3d.fromComponents ( -6, 0, 6 )

    CubicSpline3d.secondDerivative exampleSpline
        ParameterValue.one
    --> Vector3d.fromComponents ( 0, -12, 12 )

-}
secondDerivative : CubicSpline3d -> ParameterValue -> Vector3d
secondDerivative spline parameterValue =
    let
        t =
            ParameterValue.value parameterValue

        p1 =
            startPoint spline

        p2 =
            startControlPoint spline

        p3 =
            endControlPoint spline

        p4 =
            endPoint spline

        u1 =
            Vector3d.from p1 p2

        u2 =
            Vector3d.from p2 p3

        u3 =
            Vector3d.from p3 p4

        v1 =
            Vector3d.difference u2 u1

        v2 =
            Vector3d.difference u3 u2
    in
    Vector3d.scaleBy 6 (Vector3d.interpolateFrom v1 v2 t)


{-| Evaluate the second derivative of a spline at a range of parameter values.

    exampleSpline
        |> CubicSpline3d.secondDerivativesAt
            (ParameterValue.steps 2)
    --> [ Vector3d.fromComponents ( -12, 12, 0 )
    --> , Vector3d.fromComponents ( -6, 0, 6 )
    --> , Vector3d.fromComponents ( 0, -12, 12 )
    --> ]

-}
secondDerivativesAt : List ParameterValue -> CubicSpline3d -> List Vector3d
secondDerivativesAt parameterValues spline =
    List.map (secondDerivative spline) parameterValues


{-| Get the third derivative of a spline (for a cubic spline, this is a
constant).
-}
thirdDerivative : CubicSpline3d -> Vector3d
thirdDerivative spline =
    let
        p1 =
            startPoint spline

        p2 =
            startControlPoint spline

        p3 =
            endControlPoint spline

        p4 =
            endPoint spline

        u1 =
            Vector3d.from p1 p2

        u2 =
            Vector3d.from p2 p3

        u3 =
            Vector3d.from p3 p4

        v1 =
            Vector3d.difference u2 u1

        v2 =
            Vector3d.difference u3 u2
    in
    Vector3d.scaleBy 6 (Vector3d.difference v2 v1)


{-| Find a conservative upper bound on the magnitude of the second derivative of
a spline. This can be useful when determining error bounds for various kinds of
linear approximations.
-}
maxSecondDerivativeMagnitude : CubicSpline3d -> Float
maxSecondDerivativeMagnitude spline =
    let
        p1 =
            startPoint spline

        p2 =
            startControlPoint spline

        p3 =
            endControlPoint spline

        p4 =
            endPoint spline

        u1 =
            Vector3d.from p1 p2

        u2 =
            Vector3d.from p2 p3

        u3 =
            Vector3d.from p3 p4

        v1 =
            Vector3d.difference u2 u1

        v2 =
            Vector3d.difference u3 u2
    in
    6 * max (Vector3d.length v1) (Vector3d.length v2)


derivativeMagnitude : CubicSpline3d -> ParameterValue -> Float
derivativeMagnitude spline =
    let
        p1 =
            startPoint spline

        p2 =
            startControlPoint spline

        p3 =
            endControlPoint spline

        p4 =
            endPoint spline

        ( x1, y1, z1 ) =
            Point3d.coordinates p1

        ( x2, y2, z2 ) =
            Point3d.coordinates p2

        ( x3, y3, z3 ) =
            Point3d.coordinates p3

        ( x4, y4, z4 ) =
            Point3d.coordinates p4

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

        x34 =
            x4 - x3

        y34 =
            y4 - y3

        z34 =
            z4 - z3

        x123 =
            x23 - x12

        y123 =
            y23 - y12

        z123 =
            z23 - z12

        x234 =
            x34 - x23

        y234 =
            y34 - y23

        z234 =
            z34 - z23
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

            x24 =
                x23 + t * x234

            y24 =
                y23 + t * y234

            z24 =
                z23 + t * z234

            x14 =
                x13 + t * (x24 - x13)

            y14 =
                y13 + t * (y24 - y13)

            z14 =
                z13 + t * (z24 - z13)
        in
        3 * sqrt (x14 * x14 + y14 * y14 + z14 * z14)
