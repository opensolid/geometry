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


{-| -}
with : { startPoint : Point3d, startControlPoint : Point3d, endControlPoint : Point3d, endPoint : Point3d } -> CubicSpline3d
with =
    Types.CubicSpline3d


{-| -}
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


{-| -}
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


{-| -}
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


{-| -}
startPoint : CubicSpline3d -> Point3d
startPoint (Types.CubicSpline3d spline) =
    spline.startPoint


{-| -}
endPoint : CubicSpline3d -> Point3d
endPoint (Types.CubicSpline3d spline) =
    spline.endPoint


{-| -}
startControlPoint : CubicSpline3d -> Point3d
startControlPoint (Types.CubicSpline3d spline) =
    spline.startControlPoint


{-| -}
endControlPoint : CubicSpline3d -> Point3d
endControlPoint (Types.CubicSpline3d spline) =
    spline.endControlPoint


{-| -}
startDerivative : CubicSpline3d -> Vector3d
startDerivative spline =
    Vector3d.from (startPoint spline) (startControlPoint spline)
        |> Vector3d.scaleBy 3


{-| -}
endDerivative : CubicSpline3d -> Vector3d
endDerivative spline =
    Vector3d.from (endControlPoint spline) (endPoint spline)
        |> Vector3d.scaleBy 3


{-| -}
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


{-| -}
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


{-| -}
pointsAt : List ParameterValue -> CubicSpline3d -> List Point3d
pointsAt parameterValues spline =
    List.map (pointOn spline) parameterValues


{-| -}
type Nondegenerate
    = NonZeroThirdDerivative CubicSpline3d Direction3d
    | NonZeroSecondDerivative CubicSpline3d Direction3d
    | NonZeroFirstDerivative CubicSpline3d Direction3d


{-| -}
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


{-| -}
fromNondegenerate : Nondegenerate -> CubicSpline3d
fromNondegenerate nondegenerateSpline =
    case nondegenerateSpline of
        NonZeroThirdDerivative spline _ ->
            spline

        NonZeroSecondDerivative spline _ ->
            spline

        NonZeroFirstDerivative spline _ ->
            spline


{-| -}
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


{-| -}
tangentDirectionsAt : List ParameterValue -> Nondegenerate -> List Direction3d
tangentDirectionsAt parameterValues nondegenerateSpline =
    List.map (tangentDirection nondegenerateSpline) parameterValues


{-| -}
sample : Nondegenerate -> ParameterValue -> ( Point3d, Direction3d )
sample nondegenerateSpline parameterValue =
    ( pointOn (fromNondegenerate nondegenerateSpline) parameterValue
    , tangentDirection nondegenerateSpline parameterValue
    )


{-| -}
samplesAt : List ParameterValue -> Nondegenerate -> List ( Point3d, Direction3d )
samplesAt parameterValues nondegenerateSpline =
    List.map (sample nondegenerateSpline) parameterValues


{-| -}
reverse : CubicSpline3d -> CubicSpline3d
reverse spline =
    with
        { startPoint = endPoint spline
        , startControlPoint = endControlPoint spline
        , endControlPoint = startControlPoint spline
        , endPoint = startPoint spline
        }


{-| -}
scaleAbout : Point3d -> Float -> CubicSpline3d -> CubicSpline3d
scaleAbout point scale =
    mapControlPoints (Point3d.scaleAbout point scale)


{-| -}
rotateAround : Axis3d -> Float -> CubicSpline3d -> CubicSpline3d
rotateAround axis angle =
    mapControlPoints (Point3d.rotateAround axis angle)


{-| -}
translateBy : Vector3d -> CubicSpline3d -> CubicSpline3d
translateBy displacement =
    mapControlPoints (Point3d.translateBy displacement)


{-| -}
translateIn : Direction3d -> Float -> CubicSpline3d -> CubicSpline3d
translateIn direction distance spline =
    translateBy (Vector3d.withLength distance direction) spline


{-| -}
mirrorAcross : Plane3d -> CubicSpline3d -> CubicSpline3d
mirrorAcross plane =
    mapControlPoints (Point3d.mirrorAcross plane)


{-| -}
projectOnto : Plane3d -> CubicSpline3d -> CubicSpline3d
projectOnto plane =
    mapControlPoints (Point3d.projectOnto plane)


{-| -}
relativeTo : Frame3d -> CubicSpline3d -> CubicSpline3d
relativeTo frame =
    mapControlPoints (Point3d.relativeTo frame)


{-| -}
placeIn : Frame3d -> CubicSpline3d -> CubicSpline3d
placeIn frame =
    mapControlPoints (Point3d.placeIn frame)


{-| -}
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


{-| -}
bisect : CubicSpline3d -> ( CubicSpline3d, CubicSpline3d )
bisect =
    splitAt ParameterValue.half


{-| -}
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


{-| -}
type ArcLengthParameterized
    = ArcLengthParameterized
        { underlyingSpline : CubicSpline3d
        , parameterization : ArcLengthParameterization
        , nondegenerateSpline : Maybe Nondegenerate
        }


{-| -}
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


{-| -}
arcLength : ArcLengthParameterized -> Float
arcLength parameterizedSpline =
    arcLengthParameterization parameterizedSpline
        |> ArcLengthParameterization.totalArcLength


{-| -}
pointAlong : ArcLengthParameterized -> Float -> Maybe Point3d
pointAlong (ArcLengthParameterized parameterized) distance =
    parameterized.parameterization
        |> ArcLengthParameterization.arcLengthToParameterValue distance
        |> Maybe.map (pointOn parameterized.underlyingSpline)


{-| -}
tangentDirectionAlong : ArcLengthParameterized -> Float -> Maybe Direction3d
tangentDirectionAlong (ArcLengthParameterized parameterized) distance =
    case parameterized.nondegenerateSpline of
        Just nondegenerateSpline ->
            parameterized.parameterization
                |> ArcLengthParameterization.arcLengthToParameterValue distance
                |> Maybe.map (tangentDirection nondegenerateSpline)

        Nothing ->
            Nothing


{-| -}
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


{-| -}
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


{-| -}
firstDerivativesAt : List ParameterValue -> CubicSpline3d -> List Vector3d
firstDerivativesAt parameterValues spline =
    List.map (firstDerivative spline) parameterValues


{-| -}
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


{-| -}
secondDerivativesAt : List ParameterValue -> CubicSpline3d -> List Vector3d
secondDerivativesAt parameterValues spline =
    List.map (secondDerivative spline) parameterValues


{-| -}
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


{-| -}
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
