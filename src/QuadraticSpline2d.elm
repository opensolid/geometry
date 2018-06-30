module QuadraticSpline2d
    exposing
        ( ArcLengthParameterized
        , Nondegenerate
        , QuadraticSpline2d
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
        , placeIn
        , pointAlong
        , pointOn
        , pointsAt
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

{-| <img src="https://ianmackenzie.github.io/elm-geometry/1.0.0/QuadraticSpline2d/icon.svg" alt="QuadraticSpline2d" width="160">

A `QuadraticSpline2d` is a quadratic [Bézier curve](https://en.wikipedia.org/wiki/B%C3%A9zier_curve)
in 2D defined by a start point, control point and end point. This module
contains functionality for

  - Evaluating points and derivatives along a spline
  - Scaling, rotating, translating or mirroring a spline
  - Converting a spline between local and global coordinates in different
    reference frames

@docs QuadraticSpline2d


# Constructors

@docs with


# Properties

@docs startPoint, endPoint, controlPoint, startDerivative, endDerivative, boundingBox


# Evaluation

@docs pointOn, pointsAt
@docs Nondegenerate, nondegenerate, fromNondegenerate
@docs tangentDirection, tangentDirectionsAt, sample, samplesAt


# Transformations

@docs reverse, scaleAbout, rotateAround, translateBy, translateIn, mirrorAcross


# Coordinate conversions

@docs relativeTo, placeIn


# Subdivision

@docs bisect, splitAt


# Arc length parameterization

@docs ArcLengthParameterized, arcLengthParameterized, arcLength, pointAlong, tangentDirectionAlong, sampleAlong


## Low level

An `ArcLengthParameterized` value is a combination of an
[`ArcLengthParameterization`](Geometry-ArcLengthParameterization) and an
underlying `QuadraticSpline2d`. If you need to do something fancy, you can
extract these two values separately.

@docs arcLengthParameterization, fromArcLengthParameterized


# Differentiation

You are unlikely to need to use these functions directly, but they are useful if
you are writing low-level geometric algorithms.

@docs firstDerivative, firstDerivativesAt, secondDerivative

-}

import Axis2d exposing (Axis2d)
import BoundingBox2d exposing (BoundingBox2d)
import Curve.ArcLengthParameterization as ArcLengthParameterization exposing (ArcLengthParameterization)
import Curve.ParameterValue as ParameterValue exposing (ParameterValue)
import Direction2d exposing (Direction2d)
import Frame2d exposing (Frame2d)
import Geometry.Types as Types
import Point2d exposing (Point2d)
import Vector2d exposing (Vector2d)


{-| -}
type alias QuadraticSpline2d =
    Types.QuadraticSpline2d


{-| -}
with : { startPoint : Point2d, controlPoint : Point2d, endPoint : Point2d } -> QuadraticSpline2d
with =
    Types.QuadraticSpline2d


{-| -}
startPoint : QuadraticSpline2d -> Point2d
startPoint (Types.QuadraticSpline2d spline) =
    spline.startPoint


{-| -}
endPoint : QuadraticSpline2d -> Point2d
endPoint (Types.QuadraticSpline2d spline) =
    spline.endPoint


{-| -}
controlPoint : QuadraticSpline2d -> Point2d
controlPoint (Types.QuadraticSpline2d spline) =
    spline.controlPoint


{-| -}
startDerivative : QuadraticSpline2d -> Vector2d
startDerivative spline =
    Vector2d.from (startPoint spline) (controlPoint spline)
        |> Vector2d.scaleBy 2


{-| -}
endDerivative : QuadraticSpline2d -> Vector2d
endDerivative spline =
    Vector2d.from (controlPoint spline) (endPoint spline)
        |> Vector2d.scaleBy 2


{-| -}
boundingBox : QuadraticSpline2d -> BoundingBox2d
boundingBox spline =
    let
        ( x1, y1 ) =
            Point2d.coordinates (startPoint spline)

        ( x2, y2 ) =
            Point2d.coordinates (controlPoint spline)

        ( x3, y3 ) =
            Point2d.coordinates (endPoint spline)
    in
    BoundingBox2d.fromExtrema
        { minX = min x1 (min x2 x3)
        , maxX = max x1 (max x2 x3)
        , minY = min y1 (min y2 y3)
        , maxY = max y1 (max y2 y3)
        }


{-| -}
pointOn : QuadraticSpline2d -> ParameterValue -> Point2d
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
            Point2d.interpolateFrom p1 p2 t

        q2 =
            Point2d.interpolateFrom p2 p3 t
    in
    Point2d.interpolateFrom q1 q2 t


{-| -}
pointsAt : List ParameterValue -> QuadraticSpline2d -> List Point2d
pointsAt parameterValues spline =
    List.map (pointOn spline) parameterValues


{-| -}
firstDerivative : QuadraticSpline2d -> ParameterValue -> Vector2d
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
            Vector2d.from p1 p2

        v2 =
            Vector2d.from p2 p3
    in
    Vector2d.interpolateFrom v1 v2 t |> Vector2d.scaleBy 2


{-| -}
firstDerivativesAt : List ParameterValue -> QuadraticSpline2d -> List Vector2d
firstDerivativesAt parameterValues spline =
    List.map (firstDerivative spline) parameterValues


derivativeMagnitude : QuadraticSpline2d -> ParameterValue -> Float
derivativeMagnitude spline =
    let
        ( x1, y1 ) =
            Point2d.coordinates (startPoint spline)

        ( x2, y2 ) =
            Point2d.coordinates (controlPoint spline)

        ( x3, y3 ) =
            Point2d.coordinates (endPoint spline)

        x12 =
            x2 - x1

        y12 =
            y2 - y1

        x23 =
            x3 - x2

        y23 =
            y3 - y2

        x123 =
            x23 - x12

        y123 =
            y23 - y12
    in
    \parameterValue ->
        let
            t =
                ParameterValue.value parameterValue

            x13 =
                x12 + t * x123

            y13 =
                y12 + t * y123
        in
        2 * sqrt (x13 * x13 + y13 * y13)


{-| -}
type Nondegenerate
    = NonZeroSecondDerivative QuadraticSpline2d Direction2d
    | NonZeroFirstDerivative QuadraticSpline2d Direction2d


{-| -}
nondegenerate : QuadraticSpline2d -> Result Point2d Nondegenerate
nondegenerate spline =
    case Vector2d.direction (secondDerivative spline) of
        Just direction ->
            Ok (NonZeroSecondDerivative spline direction)

        Nothing ->
            let
                -- Second derivative is zero, so first derivative is constant -
                -- evaluate it at an arbitrary point to get its value
                firstDerivativeVector =
                    firstDerivative spline ParameterValue.zero
            in
            case Vector2d.direction firstDerivativeVector of
                Just direction ->
                    Ok (NonZeroFirstDerivative spline direction)

                Nothing ->
                    Err (startPoint spline)


{-| -}
fromNondegenerate : Nondegenerate -> QuadraticSpline2d
fromNondegenerate nondegenerateSpline =
    case nondegenerateSpline of
        NonZeroSecondDerivative spline _ ->
            spline

        NonZeroFirstDerivative spline _ ->
            spline


{-| -}
tangentDirection : Nondegenerate -> ParameterValue -> Direction2d
tangentDirection nondegenerateSpline parameterValue =
    case nondegenerateSpline of
        NonZeroSecondDerivative spline secondDerivativeDirection ->
            let
                firstDerivativeVector =
                    firstDerivative spline parameterValue
            in
            case Vector2d.direction firstDerivativeVector of
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
                        Direction2d.reverse secondDerivativeDirection
                    else
                        secondDerivativeDirection

        NonZeroFirstDerivative spline firstDerivativeDirection ->
            -- Tangent direction is always equal to the (constant) first
            -- derivative direction
            firstDerivativeDirection


{-| -}
tangentDirectionsAt : List ParameterValue -> Nondegenerate -> List Direction2d
tangentDirectionsAt parameterValues nondegenerateSpline =
    List.map (tangentDirection nondegenerateSpline) parameterValues


{-| -}
sample : Nondegenerate -> ParameterValue -> ( Point2d, Direction2d )
sample nondegenerateSpline parameterValue =
    ( pointOn (fromNondegenerate nondegenerateSpline) parameterValue
    , tangentDirection nondegenerateSpline parameterValue
    )


{-| -}
samplesAt : List ParameterValue -> Nondegenerate -> List ( Point2d, Direction2d )
samplesAt parameterValues nondegenerateSpline =
    List.map (sample nondegenerateSpline) parameterValues


{-| -}
reverse : QuadraticSpline2d -> QuadraticSpline2d
reverse spline =
    with
        { startPoint = endPoint spline
        , controlPoint = controlPoint spline
        , endPoint = startPoint spline
        }


{-| -}
scaleAbout : Point2d -> Float -> QuadraticSpline2d -> QuadraticSpline2d
scaleAbout point scale =
    mapControlPoints (Point2d.scaleAbout point scale)


{-| -}
rotateAround : Point2d -> Float -> QuadraticSpline2d -> QuadraticSpline2d
rotateAround point angle =
    mapControlPoints (Point2d.rotateAround point angle)


{-| -}
translateBy : Vector2d -> QuadraticSpline2d -> QuadraticSpline2d
translateBy displacement =
    mapControlPoints (Point2d.translateBy displacement)


{-| -}
translateIn : Direction2d -> Float -> QuadraticSpline2d -> QuadraticSpline2d
translateIn direction distance spline =
    translateBy (Vector2d.withLength distance direction) spline


{-| -}
mirrorAcross : Axis2d -> QuadraticSpline2d -> QuadraticSpline2d
mirrorAcross axis =
    mapControlPoints (Point2d.mirrorAcross axis)


{-| -}
relativeTo : Frame2d -> QuadraticSpline2d -> QuadraticSpline2d
relativeTo frame =
    mapControlPoints (Point2d.relativeTo frame)


{-| -}
placeIn : Frame2d -> QuadraticSpline2d -> QuadraticSpline2d
placeIn frame =
    mapControlPoints (Point2d.placeIn frame)


mapControlPoints : (Point2d -> Point2d) -> QuadraticSpline2d -> QuadraticSpline2d
mapControlPoints function spline =
    with
        { startPoint = function (startPoint spline)
        , controlPoint = function (controlPoint spline)
        , endPoint = function (endPoint spline)
        }


{-| -}
bisect : QuadraticSpline2d -> ( QuadraticSpline2d, QuadraticSpline2d )
bisect =
    splitAt ParameterValue.half


{-| -}
splitAt : ParameterValue -> QuadraticSpline2d -> ( QuadraticSpline2d, QuadraticSpline2d )
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
            Point2d.interpolateFrom p1 p2 t

        q2 =
            Point2d.interpolateFrom p2 p3 t

        r =
            Point2d.interpolateFrom q1 q2 t
    in
    ( with { startPoint = p1, controlPoint = q1, endPoint = r }
    , with { startPoint = r, controlPoint = q2, endPoint = p3 }
    )


{-| -}
type ArcLengthParameterized
    = ArcLengthParameterized
        { underlyingSpline : QuadraticSpline2d
        , parameterization : ArcLengthParameterization
        , nondegenerateSpline : Maybe Nondegenerate
        }


{-| -}
arcLengthParameterized : { maxError : Float } -> QuadraticSpline2d -> ArcLengthParameterized
arcLengthParameterized { maxError } spline =
    let
        parameterization =
            ArcLengthParameterization.build
                { maxError = maxError
                , derivativeMagnitude = derivativeMagnitude spline
                , maxSecondDerivativeMagnitude =
                    Vector2d.length (secondDerivative spline)
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
pointAlong : ArcLengthParameterized -> Float -> Maybe Point2d
pointAlong (ArcLengthParameterized parameterized) distance =
    parameterized.parameterization
        |> ArcLengthParameterization.arcLengthToParameterValue distance
        |> Maybe.map (pointOn parameterized.underlyingSpline)


{-| -}
tangentDirectionAlong : ArcLengthParameterized -> Float -> Maybe Direction2d
tangentDirectionAlong (ArcLengthParameterized parameterized) distance =
    case parameterized.nondegenerateSpline of
        Just nondegenerateSpline ->
            parameterized.parameterization
                |> ArcLengthParameterization.arcLengthToParameterValue distance
                |> Maybe.map (tangentDirection nondegenerateSpline)

        Nothing ->
            Nothing


{-| -}
sampleAlong : ArcLengthParameterized -> Float -> Maybe ( Point2d, Direction2d )
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
fromArcLengthParameterized : ArcLengthParameterized -> QuadraticSpline2d
fromArcLengthParameterized (ArcLengthParameterized parameterized) =
    parameterized.underlyingSpline


{-| -}
secondDerivative : QuadraticSpline2d -> Vector2d
secondDerivative spline =
    let
        p1 =
            startPoint spline

        p2 =
            controlPoint spline

        p3 =
            endPoint spline

        v1 =
            Vector2d.from p1 p2

        v2 =
            Vector2d.from p2 p3
    in
    Vector2d.difference v2 v1 |> Vector2d.scaleBy 2
