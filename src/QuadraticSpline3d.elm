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


{-| -}
with : { startPoint : Point3d, controlPoint : Point3d, endPoint : Point3d } -> QuadraticSpline3d
with =
    Types.QuadraticSpline3d


{-| -}
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


{-| -}
startPoint : QuadraticSpline3d -> Point3d
startPoint (Types.QuadraticSpline3d spline) =
    spline.startPoint


{-| -}
endPoint : QuadraticSpline3d -> Point3d
endPoint (Types.QuadraticSpline3d spline) =
    spline.endPoint


{-| -}
controlPoint : QuadraticSpline3d -> Point3d
controlPoint (Types.QuadraticSpline3d spline) =
    spline.controlPoint


{-| -}
startDerivative : QuadraticSpline3d -> Vector3d
startDerivative spline =
    Vector3d.from (startPoint spline) (controlPoint spline)
        |> Vector3d.scaleBy 2


{-| -}
endDerivative : QuadraticSpline3d -> Vector3d
endDerivative spline =
    Vector3d.from (controlPoint spline) (endPoint spline)
        |> Vector3d.scaleBy 2


{-| -}
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


{-| -}
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


{-| -}
pointsAt : List ParameterValue -> QuadraticSpline3d -> List Point3d
pointsAt parameterValues spline =
    List.map (pointOn spline) parameterValues


{-| -}
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


{-| -}
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


{-| -}
type Nondegenerate
    = NonZeroSecondDerivative QuadraticSpline3d Direction3d
    | NonZeroFirstDerivative QuadraticSpline3d Direction3d


{-| -}
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


{-| -}
fromNondegenerate : Nondegenerate -> QuadraticSpline3d
fromNondegenerate nondegenerateSpline =
    case nondegenerateSpline of
        NonZeroSecondDerivative spline _ ->
            spline

        NonZeroFirstDerivative spline _ ->
            spline


{-| -}
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
reverse : QuadraticSpline3d -> QuadraticSpline3d
reverse spline =
    with
        { startPoint = endPoint spline
        , controlPoint = controlPoint spline
        , endPoint = startPoint spline
        }


{-| -}
scaleAbout : Point3d -> Float -> QuadraticSpline3d -> QuadraticSpline3d
scaleAbout point scale =
    mapControlPoints (Point3d.scaleAbout point scale)


{-| -}
rotateAround : Axis3d -> Float -> QuadraticSpline3d -> QuadraticSpline3d
rotateAround axis angle =
    mapControlPoints (Point3d.rotateAround axis angle)


{-| -}
translateBy : Vector3d -> QuadraticSpline3d -> QuadraticSpline3d
translateBy displacement =
    mapControlPoints (Point3d.translateBy displacement)


{-| -}
translateIn : Direction3d -> Float -> QuadraticSpline3d -> QuadraticSpline3d
translateIn direction distance spline =
    translateBy (Vector3d.withLength distance direction) spline


{-| -}
mirrorAcross : Plane3d -> QuadraticSpline3d -> QuadraticSpline3d
mirrorAcross plane =
    mapControlPoints (Point3d.mirrorAcross plane)


{-| -}
projectOnto : Plane3d -> QuadraticSpline3d -> QuadraticSpline3d
projectOnto plane =
    mapControlPoints (Point3d.projectOnto plane)


{-| -}
relativeTo : Frame3d -> QuadraticSpline3d -> QuadraticSpline3d
relativeTo frame =
    mapControlPoints (Point3d.relativeTo frame)


{-| -}
placeIn : Frame3d -> QuadraticSpline3d -> QuadraticSpline3d
placeIn frame =
    mapControlPoints (Point3d.placeIn frame)


{-| -}
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


{-| -}
bisect : QuadraticSpline3d -> ( QuadraticSpline3d, QuadraticSpline3d )
bisect =
    splitAt ParameterValue.half


{-| -}
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


{-| -}
type ArcLengthParameterized
    = ArcLengthParameterized
        { underlyingSpline : QuadraticSpline3d
        , parameterization : ArcLengthParameterization
        , nondegenerateSpline : Maybe Nondegenerate
        }


{-| -}
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
fromArcLengthParameterized : ArcLengthParameterized -> QuadraticSpline3d
fromArcLengthParameterized (ArcLengthParameterized parameterized) =
    parameterized.underlyingSpline


{-| -}
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
