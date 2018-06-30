module CubicSpline2d
    exposing
        ( ArcLengthParameterized
        , CubicSpline2d
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

{-| <img src="https://ianmackenzie.github.io/elm-geometry/1.0.0/CubicSpline2d/icon.svg" alt="CubicSpline2d" width="160">

A `CubicSpline2d` is a cubic [Bézier curve](https://en.wikipedia.org/wiki/B%C3%A9zier_curve)
in 2D defined by a start point, end point and two control points. This module
contains functionality for

  - Constructing splines
  - Evaluating points and tangent directions along a spline
  - Scaling, rotating, translating or mirroring a spline
  - Converting a spline between local and global coordinates in different
    reference frames

@docs CubicSpline2d


# Constructors

@docs with, fromEndpoints, fromQuadraticSpline


# Properties

@docs startPoint, endPoint, startControlPoint, endControlPoint, startDerivative, endDerivative, boundingBox


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
underlying `CubicSpline2d`. If you need to do something fancy, you can extract
these two values separately.

@docs arcLengthParameterization, fromArcLengthParameterized


# Differentiation

You are unlikely to need to use these functions directly, but they are useful if
you are writing low-level geometric algorithms.

@docs firstDerivative, firstDerivativesAt, secondDerivative, secondDerivativesAt, thirdDerivative, maxSecondDerivativeMagnitude

-}

import Axis2d exposing (Axis2d)
import BoundingBox2d exposing (BoundingBox2d)
import Curve.ArcLengthParameterization as ArcLengthParameterization exposing (ArcLengthParameterization)
import Curve.ParameterValue as ParameterValue exposing (ParameterValue)
import Direction2d exposing (Direction2d)
import Frame2d exposing (Frame2d)
import Geometry.Types as Types
import Point2d exposing (Point2d)
import QuadraticSpline2d exposing (QuadraticSpline2d)
import Vector2d exposing (Vector2d)


{-| -}
type alias CubicSpline2d =
    Types.CubicSpline2d


{-| -}
with : { startPoint : Point2d, startControlPoint : Point2d, endControlPoint : Point2d, endPoint : Point2d } -> CubicSpline2d
with =
    Types.CubicSpline2d


{-| -}
fromEndpoints : { startPoint : Point2d, startDerivative : Vector2d, endPoint : Point2d, endDerivative : Vector2d } -> CubicSpline2d
fromEndpoints arguments =
    let
        startControlPoint_ =
            arguments.startPoint
                |> Point2d.translateBy
                    (Vector2d.scaleBy (1 / 3) arguments.startDerivative)

        endControlPoint_ =
            arguments.endPoint
                |> Point2d.translateBy
                    (Vector2d.scaleBy (-1 / 3) arguments.endDerivative)
    in
    with
        { startPoint = arguments.startPoint
        , startControlPoint = startControlPoint_
        , endControlPoint = endControlPoint_
        , endPoint = arguments.endPoint
        }


{-| -}
fromQuadraticSpline : QuadraticSpline2d -> CubicSpline2d
fromQuadraticSpline quadraticSpline =
    let
        startPoint_ =
            QuadraticSpline2d.startPoint quadraticSpline

        controlPoint_ =
            QuadraticSpline2d.controlPoint quadraticSpline

        endPoint_ =
            QuadraticSpline2d.endPoint quadraticSpline

        startControlPoint_ =
            Point2d.interpolateFrom startPoint_ controlPoint_ (2 / 3)

        endControlPoint_ =
            Point2d.interpolateFrom endPoint_ controlPoint_ (2 / 3)
    in
    with
        { startPoint = startPoint_
        , startControlPoint = startControlPoint_
        , endControlPoint = endControlPoint_
        , endPoint = endPoint_
        }


{-| -}
startPoint : CubicSpline2d -> Point2d
startPoint (Types.CubicSpline2d spline) =
    spline.startPoint


{-| -}
endPoint : CubicSpline2d -> Point2d
endPoint (Types.CubicSpline2d spline) =
    spline.endPoint


{-| -}
startControlPoint : CubicSpline2d -> Point2d
startControlPoint (Types.CubicSpline2d spline) =
    spline.startControlPoint


{-| -}
endControlPoint : CubicSpline2d -> Point2d
endControlPoint (Types.CubicSpline2d spline) =
    spline.endControlPoint


{-| -}
startDerivative : CubicSpline2d -> Vector2d
startDerivative spline =
    Vector2d.from (startPoint spline) (startControlPoint spline)
        |> Vector2d.scaleBy 3


{-| -}
endDerivative : CubicSpline2d -> Vector2d
endDerivative spline =
    Vector2d.from (endControlPoint spline) (endPoint spline)
        |> Vector2d.scaleBy 3


{-| -}
boundingBox : CubicSpline2d -> BoundingBox2d
boundingBox spline =
    let
        ( x1, y1 ) =
            Point2d.coordinates (startPoint spline)

        ( x2, y2 ) =
            Point2d.coordinates (startControlPoint spline)

        ( x3, y3 ) =
            Point2d.coordinates (endControlPoint spline)

        ( x4, y4 ) =
            Point2d.coordinates (endPoint spline)
    in
    BoundingBox2d.fromExtrema
        { minX = min (min x1 x2) (min x3 x4)
        , maxX = max (max x1 x2) (max x3 x4)
        , minY = min (min y1 y2) (min y3 y4)
        , maxY = max (max y1 y2) (max y3 y4)
        }


{-| -}
pointOn : CubicSpline2d -> ParameterValue -> Point2d
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
            Point2d.interpolateFrom p1 p2 t

        q2 =
            Point2d.interpolateFrom p2 p3 t

        q3 =
            Point2d.interpolateFrom p3 p4 t

        r1 =
            Point2d.interpolateFrom q1 q2 t

        r2 =
            Point2d.interpolateFrom q2 q3 t
    in
    Point2d.interpolateFrom r1 r2 t


{-| -}
pointsAt : List ParameterValue -> CubicSpline2d -> List Point2d
pointsAt parameterValues spline =
    List.map (pointOn spline) parameterValues


{-| -}
type Nondegenerate
    = NonZeroThirdDerivative CubicSpline2d Direction2d
    | NonZeroSecondDerivative CubicSpline2d Direction2d
    | NonZeroFirstDerivative CubicSpline2d Direction2d


{-| -}
nondegenerate : CubicSpline2d -> Result Point2d Nondegenerate
nondegenerate spline =
    case Vector2d.direction (thirdDerivative spline) of
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
            case Vector2d.direction secondDerivativeVector of
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
                    case Vector2d.direction firstDerivativeVector of
                        Just direction ->
                            -- First derivative is constant and non-zero, so the
                            -- tangent direction will always be equal to the
                            -- first derivative direction
                            Ok (NonZeroFirstDerivative spline direction)

                        Nothing ->
                            Err (startPoint spline)


{-| -}
fromNondegenerate : Nondegenerate -> CubicSpline2d
fromNondegenerate nondegenerateSpline =
    case nondegenerateSpline of
        NonZeroThirdDerivative spline _ ->
            spline

        NonZeroSecondDerivative spline _ ->
            spline

        NonZeroFirstDerivative spline _ ->
            spline


{-| -}
tangentDirection : Nondegenerate -> ParameterValue -> Direction2d
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
            case Vector2d.direction firstDerivativeVector of
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
                        Direction2d.reverse secondDerivativeDirection
                    else
                        secondDerivativeDirection

        NonZeroThirdDerivative spline thirdDerivativeDirection ->
            let
                firstDerivativeVector =
                    firstDerivative spline parameterValue
            in
            case Vector2d.direction firstDerivativeVector of
                Just firstDerivativeDirection ->
                    -- First derivative is non-zero, so use its direction as the
                    -- tangent direction (normal case)
                    firstDerivativeDirection

                Nothing ->
                    let
                        secondDerivativeVector =
                            secondDerivative spline parameterValue
                    in
                    case Vector2d.direction secondDerivativeVector of
                        Just secondDerivativeDirection ->
                            -- Zero first derivative and non-zero second
                            -- derivative mean we have reached a reversal point,
                            -- as above in the NonZeroSecondDerivative case
                            if parameterValue == ParameterValue.one then
                                Direction2d.reverse secondDerivativeDirection
                            else
                                secondDerivativeDirection

                        Nothing ->
                            -- First and second derivatives are zero, so fall
                            -- back to the third derivative direction
                            thirdDerivativeDirection


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
reverse : CubicSpline2d -> CubicSpline2d
reverse spline =
    with
        { startPoint = endPoint spline
        , startControlPoint = endControlPoint spline
        , endControlPoint = startControlPoint spline
        , endPoint = startPoint spline
        }


{-| -}
scaleAbout : Point2d -> Float -> CubicSpline2d -> CubicSpline2d
scaleAbout point scale =
    mapControlPoints (Point2d.scaleAbout point scale)


{-| -}
rotateAround : Point2d -> Float -> CubicSpline2d -> CubicSpline2d
rotateAround point angle =
    mapControlPoints (Point2d.rotateAround point angle)


{-| -}
translateBy : Vector2d -> CubicSpline2d -> CubicSpline2d
translateBy displacement =
    mapControlPoints (Point2d.translateBy displacement)


{-| -}
translateIn : Direction2d -> Float -> CubicSpline2d -> CubicSpline2d
translateIn direction distance spline =
    translateBy (Vector2d.withLength distance direction) spline


{-| -}
mirrorAcross : Axis2d -> CubicSpline2d -> CubicSpline2d
mirrorAcross axis =
    mapControlPoints (Point2d.mirrorAcross axis)


{-| -}
relativeTo : Frame2d -> CubicSpline2d -> CubicSpline2d
relativeTo frame =
    mapControlPoints (Point2d.relativeTo frame)


{-| -}
placeIn : Frame2d -> CubicSpline2d -> CubicSpline2d
placeIn frame =
    mapControlPoints (Point2d.placeIn frame)


mapControlPoints : (Point2d -> Point2d) -> CubicSpline2d -> CubicSpline2d
mapControlPoints function spline =
    with
        { startPoint = function (startPoint spline)
        , startControlPoint = function (startControlPoint spline)
        , endControlPoint = function (endControlPoint spline)
        , endPoint = function (endPoint spline)
        }


{-| -}
bisect : CubicSpline2d -> ( CubicSpline2d, CubicSpline2d )
bisect =
    splitAt ParameterValue.half


{-| -}
splitAt : ParameterValue -> CubicSpline2d -> ( CubicSpline2d, CubicSpline2d )
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
            Point2d.interpolateFrom p1 p2 t

        q2 =
            Point2d.interpolateFrom p2 p3 t

        q3 =
            Point2d.interpolateFrom p3 p4 t

        r1 =
            Point2d.interpolateFrom q1 q2 t

        r2 =
            Point2d.interpolateFrom q2 q3 t

        s =
            Point2d.interpolateFrom r1 r2 t
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
        { underlyingSpline : CubicSpline2d
        , parameterization : ArcLengthParameterization
        , nondegenerateSpline : Maybe Nondegenerate
        }


{-| -}
arcLengthParameterized : { maxError : Float } -> CubicSpline2d -> ArcLengthParameterized
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
fromArcLengthParameterized : ArcLengthParameterized -> CubicSpline2d
fromArcLengthParameterized (ArcLengthParameterized parameterized) =
    parameterized.underlyingSpline


{-| -}
firstDerivative : CubicSpline2d -> ParameterValue -> Vector2d
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

        ( x1, y1 ) =
            Point2d.coordinates p1

        ( x2, y2 ) =
            Point2d.coordinates p2

        ( x3, y3 ) =
            Point2d.coordinates p3

        ( x4, y4 ) =
            Point2d.coordinates p4

        vx1 =
            x2 - x1

        vy1 =
            y2 - y1

        vx2 =
            x3 - x2

        vy2 =
            y3 - y2

        vx3 =
            x4 - x3

        vy3 =
            y4 - y3
    in
    if t <= 0.5 then
        let
            wx1 =
                vx1 + t * (vx2 - vx1)

            wy1 =
                vy1 + t * (vy2 - vy1)

            wx2 =
                vx2 + t * (vx3 - vx2)

            wy2 =
                vy2 + t * (vy3 - vy2)
        in
        Vector2d.fromComponents
            ( 3 * (wx1 + t * (wx2 - wx1))
            , 3 * (wy1 + t * (wy2 - wy1))
            )
    else
        let
            u =
                1 - t

            wx1 =
                vx2 + u * (vx1 - vx2)

            wy1 =
                vy2 + u * (vy1 - vy2)

            wx2 =
                vx3 + u * (vx2 - vx3)

            wy2 =
                vy3 + u * (vy2 - vy3)
        in
        Vector2d.fromComponents
            ( 3 * (wx2 + u * (wx1 - wx2))
            , 3 * (wy2 + u * (wy1 - wy2))
            )


{-| -}
firstDerivativesAt : List ParameterValue -> CubicSpline2d -> List Vector2d
firstDerivativesAt parameterValues spline =
    List.map (firstDerivative spline) parameterValues


{-| -}
secondDerivative : CubicSpline2d -> ParameterValue -> Vector2d
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
            Vector2d.from p1 p2

        u2 =
            Vector2d.from p2 p3

        u3 =
            Vector2d.from p3 p4

        v1 =
            Vector2d.difference u2 u1

        v2 =
            Vector2d.difference u3 u2
    in
    Vector2d.scaleBy 6 (Vector2d.interpolateFrom v1 v2 t)


{-| -}
secondDerivativesAt : List ParameterValue -> CubicSpline2d -> List Vector2d
secondDerivativesAt parameterValues spline =
    List.map (secondDerivative spline) parameterValues


{-| -}
thirdDerivative : CubicSpline2d -> Vector2d
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
            Vector2d.from p1 p2

        u2 =
            Vector2d.from p2 p3

        u3 =
            Vector2d.from p3 p4

        v1 =
            Vector2d.difference u2 u1

        v2 =
            Vector2d.difference u3 u2
    in
    Vector2d.scaleBy 6 (Vector2d.difference v2 v1)


{-| -}
maxSecondDerivativeMagnitude : CubicSpline2d -> Float
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
            Vector2d.from p1 p2

        u2 =
            Vector2d.from p2 p3

        u3 =
            Vector2d.from p3 p4

        v1 =
            Vector2d.difference u2 u1

        v2 =
            Vector2d.difference u3 u2
    in
    6 * max (Vector2d.length v1) (Vector2d.length v2)


derivativeMagnitude : CubicSpline2d -> ParameterValue -> Float
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

        ( x1, y1 ) =
            Point2d.coordinates p1

        ( x2, y2 ) =
            Point2d.coordinates p2

        ( x3, y3 ) =
            Point2d.coordinates p3

        ( x4, y4 ) =
            Point2d.coordinates p4

        x12 =
            x2 - x1

        y12 =
            y2 - y1

        x23 =
            x3 - x2

        y23 =
            y3 - y2

        x34 =
            x4 - x3

        y34 =
            y4 - y3

        x123 =
            x23 - x12

        y123 =
            y23 - y12

        x234 =
            x34 - x23

        y234 =
            y34 - y23
    in
    \parameterValue ->
        let
            t =
                ParameterValue.value parameterValue

            x13 =
                x12 + t * x123

            y13 =
                y12 + t * y123

            x24 =
                x23 + t * x234

            y24 =
                y23 + t * y234

            x14 =
                x13 + t * (x24 - x13)

            y14 =
                y13 + t * (y24 - y13)
        in
        3 * sqrt (x14 * x14 + y14 * y14)
