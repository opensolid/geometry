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


{-| Construct a spline from its start point, control point and end point:

    exampleSpline =
        QuadraticSpline2d.with
            { startPoint =
                Point2d.fromCoordinates ( 1, 1 )
            , controlPoint =
                Point2d.fromCoordinates ( 3, 4 )
            , endPoint =
                Point2d.fromCoordinates ( 5, 1 )
            }

-}
with : { startPoint : Point2d, controlPoint : Point2d, endPoint : Point2d } -> QuadraticSpline2d
with =
    Types.QuadraticSpline2d


{-| Get the start point of a spline.

    QuadraticSpline2d.startPoint exampleSpline
    --> Point2d.fromCoordinates ( 1, 1 )

-}
startPoint : QuadraticSpline2d -> Point2d
startPoint (Types.QuadraticSpline2d spline) =
    spline.startPoint


{-| Get the end point of a spline.

    QuadraticSpline2d.endPoint exampleSpline
    --> Point2d.fromCoordinates ( 5, 1 )

-}
endPoint : QuadraticSpline2d -> Point2d
endPoint (Types.QuadraticSpline2d spline) =
    spline.endPoint


{-| Get the control point of a spline.

    QuadraticSpline2d.controlPoint exampleSpline
    --> Point2d.fromCoordinates ( 3, 4 )

-}
controlPoint : QuadraticSpline2d -> Point2d
controlPoint (Types.QuadraticSpline2d spline) =
    spline.controlPoint


{-| Get the start derivative of a spline. This is equal to twice the vector from
the spline's first control point to its second.

    QuadraticSpline2d.startDerivative exampleSpline
    --> Vector2d.fromComponents ( 4, 6 )

-}
startDerivative : QuadraticSpline2d -> Vector2d
startDerivative spline =
    Vector2d.from (startPoint spline) (controlPoint spline)
        |> Vector2d.scaleBy 2


{-| Get the end derivative of a spline. This is equal to twice the vector from
the spline's second control point to its third.

    QuadraticSpline2d.endDerivative exampleSpline
    --> Vector2d.fromComponents ( 4, -6 )

-}
endDerivative : QuadraticSpline2d -> Vector2d
endDerivative spline =
    Vector2d.from (controlPoint spline) (endPoint spline)
        |> Vector2d.scaleBy 2


{-| Compute a bounding box for a given spline. It is not guaranteed that the
result will be the _smallest_ possible bounding box, since for efficiency the
bounding box is computed from the spline's control points (which cover a larger
area than the spline itself).

    QuadraticSpline2d.boundingBox exampleSpline
    --> BoundingBox2d.fromExtrema
    -->     { minX = 1
    -->     , maxX = 5
    -->     , minY = 1
    -->     , maxY = 4
    -->     }

-}
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


{-| Get the point along a spline at a given parameter value:

    QuadraticSpline2d.pointOn exampleSpline 0
    --> Point2d.fromCoordinates ( 1, 1 )

    QuadraticSpline2d.pointOn exampleSpline 0.5
    --> Point2d.fromCoordinates ( 3, 2.5 )

    QuadraticSpline2d.pointOn exampleSpline 1
    --> Point2d.fromCoordinates ( 5, 1 )

-}
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


{-| Get points along a spline at a given set of parameter values:

    exampleSpline
        |> QuadraticSpline2d.pointsAt
            (ParameterValue.steps 2)
    --> [ Point2d.fromCoordinates ( 1, 1 )
    --> , Point2d.fromCoordinates ( 3, 2.5 )
    --> , Point2d.fromCoordinates ( 5, 1 )
    --> ]

-}
pointsAt : List ParameterValue -> QuadraticSpline2d -> List Point2d
pointsAt parameterValues spline =
    List.map (pointOn spline) parameterValues


{-| Get the first derivative of a spline at a given parameter value.

    QuadraticSpline2d.derivative exampleSpline
        ParameterValue.zero
    --> Vector2d.fromComponents ( 4, 6 )

    QuadraticSpline2d.derivative exampleSpline
        ParameterValue.half
    --> Vector2d.fromComponents ( 4, 0 )

    QuadraticSpline2d.derivative exampleSpline
        ParameterValue.one
    --> Vector2d.fromComponents ( 4, -6 )

Note that the derivative interpolates linearly from end to end.

-}
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


{-| Evaluate the first derivative of a spline at a range of parameter values:

    exampleSpline
        |> QuadraticSpline2d.firstDerivativesAt
            (ParameterValue.steps 2)
    --> [ Vector2d.fromComponents ( 4, 6 )
    --> , Vector2d.fromComponents ( 4, 0 )
    --> , Vector2d.fromComponents ( 4, -6 )
    --> ]

-}
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


{-| If a curve has zero length (consists of just a single point), then we say
that it is 'degenerate'. Some operations such as computing tangent directions
are not defined on degenerate curves.

A `Nondegenerate` value represents a spline that is definitely not degenerate.
It is used as input to functions such as `QuadraticSpline2d.tangentDirection`
and can be constructed using `QuadraticSpline2d.nondegenerate`.

-}
type Nondegenerate
    = NonZeroSecondDerivative QuadraticSpline2d Direction2d
    | NonZeroFirstDerivative QuadraticSpline2d Direction2d


{-| Attempt to construct a nondegenerate spline from a general
`QuadraticSpline2d`. If the spline is in fact degenerate (consists of a single
point), returns an `Err` with that point.

    QuadraticSpline2d.nondegenerate exampleSpline
    --> Ok nondegenerateExampleSpline

-}
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


{-| Convert a nondegenerate spline back to a general `QuadraticSpline2d`.

    QuadraticSpline2d.fromNondegenerate
        nondegenerateExampleSpline
    --> exampleSpline

-}
fromNondegenerate : Nondegenerate -> QuadraticSpline2d
fromNondegenerate nondegenerateSpline =
    case nondegenerateSpline of
        NonZeroSecondDerivative spline _ ->
            spline

        NonZeroFirstDerivative spline _ ->
            spline


{-| Get the tangent direction to a nondegenerate spline at a given parameter
value:

    QuadraticSpline2d.tangentDirection
        nondegenerateExampleSpline
        ParameterValue.zero
    --> Direction2d.fromAngle (degrees 56.31)

    QuadraticSpline2d.tangentDirection
        nondegenerateExampleSpline
        ParameterValue.half
    --> Direction2d.x

    QuadraticSpline2d.tangentDirection
        nondegenerateExampleSpline
        ParameterValue.one
    --> Direction2d.fromAngle (degrees -56.31)

-}
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


{-| Get tangent directions to a nondegenerate spline at a given set of parameter
values:

    nondegenerateExampleSpline
        |> QuadraticSpline2d.tangentDirectionsAt
            (ParameterValue.steps 2)
    --> [ Direction2d.fromAngle (degrees 56.31)
    --> , Direction2d.x
    --> , Direction2d.fromAngle (degrees -56.31)
    --> ]

-}
tangentDirectionsAt : List ParameterValue -> Nondegenerate -> List Direction2d
tangentDirectionsAt parameterValues nondegenerateSpline =
    List.map (tangentDirection nondegenerateSpline) parameterValues


{-| Get both the point and tangent direction of a nondegenerate spline at a
given parameter value:

    QuadraticSpline2d.sample nondegenerateExampleSpline
        ParameterValue.half
    --> ( Point2d.fromCoordinates ( 3, 2.5 )
    --> , Direction2d.x
    --> )

-}
sample : Nondegenerate -> ParameterValue -> ( Point2d, Direction2d )
sample nondegenerateSpline parameterValue =
    ( pointOn (fromNondegenerate nondegenerateSpline) parameterValue
    , tangentDirection nondegenerateSpline parameterValue
    )


{-| Get points and tangent directions of a nondegenerate spline at a given set
of parameter values:

    nondegenerateExampleSpline
        |> QuadraticSpline2d.samplesAt
            (ParameterValue.steps 2)
    --> [ ( Point2d.fromCoordinates ( 1, 1 )
    -->   , Direction2d.fromAngle (degrees 56.31)
    -->   )
    --> , ( Point2d.fromCoordinates ( 3, 2.5 )
    -->   , Direction2d.x
    -->   )
    --> , ( Point2d.fromCoordinates ( 5, 1 )
    -->   , Direction2d.fromAngle (degrees -56.31)
    -->   )
    --> ]

-}
samplesAt : List ParameterValue -> Nondegenerate -> List ( Point2d, Direction2d )
samplesAt parameterValues nondegenerateSpline =
    List.map (sample nondegenerateSpline) parameterValues


{-| Reverse a spline so that the start point becomes the end point, and vice
versa.

    QuadraticSpline2d.reverse exampleSpline
    --> QuadraticSpline2d.with
    -->     { startPoint =
    -->         Point2d.fromCoordinates ( 5, 1 )
    -->     , controlPoint =
    -->         Point2d.fromCoordinates ( 3, 4 )
    -->     , endPoint =
    -->         Point2d.fromCoordinates ( 1, 1 )
    -->     }

-}
reverse : QuadraticSpline2d -> QuadraticSpline2d
reverse spline =
    with
        { startPoint = endPoint spline
        , controlPoint = controlPoint spline
        , endPoint = startPoint spline
        }


{-| Scale a spline about the given center point by the given scale.

    examplePolyline
        |> QuadraticSpline2d.scaleAbout Point2d.origin 2
    --> QuadraticSpline2d.with
    -->     { startPoint =
    -->         Point2d.fromCoordinates ( 2, 2 )
    -->     , controlPoint =
    -->         Point2d.fromCoordinates ( 6, 8 )
    -->     , endPoint =
    -->         Point2d.fromCoordinates ( 10, 2 )
    -->     }

-}
scaleAbout : Point2d -> Float -> QuadraticSpline2d -> QuadraticSpline2d
scaleAbout point scale =
    mapControlPoints (Point2d.scaleAbout point scale)


{-| Rotate a spline counterclockwise around a given center point by a given
angle (in radians).

    examplePolyline
        |> QuadraticSpline2d.rotateAround Point2d.origin
            (degrees 90)
    --> QuadraticSpline2d.with
    -->     { startPoint =
    -->         Point2d.fromCoordinates ( -1, 1 )
    -->     , controlPoint =
    -->         Point2d.fromCoordinates ( -4, 3 )
    -->     , endPoint =
    -->         Point2d.fromCoordinates ( -1, 5 )
    -->     }

-}
rotateAround : Point2d -> Float -> QuadraticSpline2d -> QuadraticSpline2d
rotateAround point angle =
    mapControlPoints (Point2d.rotateAround point angle)


{-| Translate a spline by a given displacement.

    displacement =
        Vector2d.fromComponents ( 2, 3 )

    exampleSpline
        |> QuadraticSpline2d.translateBy displacement
    --> QuadraticSpline2d.with
    -->     { startPoint =
    -->         Point2d.fromCoordinates ( 3, 4 )
    -->     , controlPoint =
    -->         Point2d.fromCoordinates ( 5, 7 )
    -->     , endPoint =
    -->         Point2d.fromCoordinates ( 7, 4 )
    -->     )

-}
translateBy : Vector2d -> QuadraticSpline2d -> QuadraticSpline2d
translateBy displacement =
    mapControlPoints (Point2d.translateBy displacement)


{-| Translate a spline in a given direction by a given distance;

    QuadraticSpline2d.translateIn direction distance

is equivalent to

    QuadraticSpline2d.translateBy
        (Vector2d.withLength distance direction)

-}
translateIn : Direction2d -> Float -> QuadraticSpline2d -> QuadraticSpline2d
translateIn direction distance spline =
    translateBy (Vector2d.withLength distance direction) spline


{-| Mirror a spline across an axis.

    QuadraticSpline2d.mirrorAcross Axis2d.x exampleSpline
    --> QuadraticSpline2d.with
    -->     { startPoint =
    -->         Point2d.fromCoordinates ( 1, -1 )
    -->     , controlPoint =
    -->         Point2d.fromCoordinates ( 3, -4 )
    -->     , endPoint =
    -->         Point2d.fromCoordinates ( 5, -1 )
    -->     }

-}
mirrorAcross : Axis2d -> QuadraticSpline2d -> QuadraticSpline2d
mirrorAcross axis =
    mapControlPoints (Point2d.mirrorAcross axis)


{-| Take a spline defined in global coordinates, and return it expressed in
local coordinates relative to a given reference frame.

    localFrame =
        Frame2d.atCoordinates ( 1, 2 )

    QuadraticSpline2d.relativeTo localFrame exampleSpline
    --> QuadraticSpline2d.with
    -->     { startPoint =
    -->         Point2d.fromCoordinates ( 0, -1 )
    -->     , controlPoint =
    -->         Point2d.fromCoordinates ( 2, 2 )
    -->     , endPoint =
    -->         Point2d.fromCoordinates ( 4, -1 )
    -->     }

-}
relativeTo : Frame2d -> QuadraticSpline2d -> QuadraticSpline2d
relativeTo frame =
    mapControlPoints (Point2d.relativeTo frame)


{-| Take a spline considered to be defined in local coordinates relative to a
given reference frame, and return that spline expressed in global coordinates.

    localFrame =
        Frame2d.atCoordinates ( 1, 2 )

    QuadraticSpline2d.placeIn localFrame exampleSpline
    --> QuadraticSpline2d.with
    -->     { startPoint =
    -->         Point2d.fromCoordinates ( 2, 3 )
    -->     , controlPoint =
    -->         Point2d.fromCoordinates ( 4, 6 )
    -->     , endPoint =
    -->         Point2d.fromCoordinates ( 6, 3 )
    -->     }

-}
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


{-| Split a spline into two roughly equal halves.

    QuadraticSpline2d.bisect exampleSpline
    --> ( QuadraticSpline2d.with
    -->     { startPoint =
    -->         Point2d.fromCoordinates ( 1, 1 )
    -->     , controlPoint =
    -->         Point2d.fromCoordinates ( 2, 2.5 )
    -->     , endPoint =
    -->         Point2d.fromCoordinates ( 3, 2.5 )
    -->     }
    --> , QuadraticSpline2d.with
    -->     { startPoint =
    -->         Point2d.fromCoordinates ( 3, 2.5 )
    -->     , controlPoint =
    -->         Point2d.fromCoordinates ( 4, 2.5 )
    -->     , endPoint =
    -->         Point2d.fromCoordinates ( 5, 1 )
    -->     }
    --> )

Equivalent to `QuadraticSpline2d.splitAt ParameterValue.half`.

-}
bisect : QuadraticSpline2d -> ( QuadraticSpline2d, QuadraticSpline2d )
bisect =
    splitAt ParameterValue.half


{-| Split a spline at a particular parameter value, resulting in two smaller
splines.

    parameterValue =
        ParameterValue.clamped 0.75

    QuadraticSpline2d.splitAt parameterValue exampleSpline
    --> ( QuadraticSpline2d.with
    -->     { startPoint =
    -->         Point2d.fromCoordinates ( 1, 1 )
    -->     , controlPoint =
    -->         Point2d.fromCoordinates ( 2.5, 3.25 )
    -->     , endPoint =
    -->         Point2d.fromCoordinates ( 4, 2.125 )
    -->     }
    --> , QuadraticSpline2d.with
    -->     { startPoint =
    -->         Point2d.fromCoordinates ( 4, 2.125 )
    -->     , controlPoint =
    -->         Point2d.fromCoordinates ( 4.5, 1.75 )
    -->     , endPoint =
    -->         Point2d.fromCoordinates ( 5, 1 )
    -->     }
    --> )

-}
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


{-| A spline that has been parameterized by arc length.
-}
type ArcLengthParameterized
    = ArcLengthParameterized
        { underlyingSpline : QuadraticSpline2d
        , parameterization : ArcLengthParameterization
        , nondegenerateSpline : Maybe Nondegenerate
        }


{-| Build an arc length parameterization of the given spline, with a given
accuracy. Generally speaking, all operations on the resulting
`ArcLengthParameterized` value will be accurate to within the specified maximum
error.

    parameterizedSpline =
        exampleSpline
            |> QuadraticSpline2d.arcLengthParameterized
                { maxError = 1.0e-4 }

-}
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


{-| Find the total arc length of a spline:

    QuadraticSpline2d.arcLength parameterizedSpline
    --> 5.1986

In this example, the result will be accurate to within `1.0e-4` since that was
the tolerance used when constructing `parameterizedSpline`.

-}
arcLength : ArcLengthParameterized -> Float
arcLength parameterizedSpline =
    arcLengthParameterization parameterizedSpline
        |> ArcLengthParameterization.totalArcLength


{-| Try to get the point along a spline at a given arc length. For example, to
get the point a quarter of the way along `exampleSpline`:

    QuadraticSpline2d.pointAlong parameterizedSpline
        (arcLength / 4)
    --> Just (Point2d.fromCoordinates ( 1.8350, 1.9911 ))

Note that this is not the same as evaulating at a parameter value of 1/4:

    QuadraticSpline2d.pointOn exampleSpline 0.25
    --> Point2d.fromCoordinates ( 2, 2.125 )

If the given arc length is less than zero or greater than the arc length of the
spline, returns `Nothing`.

-}
pointAlong : ArcLengthParameterized -> Float -> Maybe Point2d
pointAlong (ArcLengthParameterized parameterized) distance =
    parameterized.parameterization
        |> ArcLengthParameterization.arcLengthToParameterValue distance
        |> Maybe.map (pointOn parameterized.underlyingSpline)


{-| Try to get the tangent direction along a spline at a given arc length. To
get the tangent direction a quarter of the way along `exampleSpline`:

    QuadraticSpline2d.tangentDirectionAlong
        parameterizedSpline
        (arcLength / 4)
    --> Just (Direction2d.fromAngle (degrees 41.145))

If the given arc length is less than zero or greater than the arc length of the
spline (or if the derivative of the spline happens to be exactly zero at the
given arc length), returns `Nothing`.

-}
tangentDirectionAlong : ArcLengthParameterized -> Float -> Maybe Direction2d
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

    QuadraticSpline2d.sampleAlong parameterizedSpline
        (0.25 * arcLength)
    --> Just
    -->     ( Point2d.fromCoordinates ( 1.8350, 1.9911 )
    -->     , Direction2d.fromAngle (degrees 41.145)
    -->     )

If the given arc length is less than zero or greater than the arc length of the
spline (or if the spline is degenerate), returns `Nothing`.

-}
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


{-| Get the original `QuadraticSpline2d` from which an `ArcLengthParameterized`
value was constructed.
-}
fromArcLengthParameterized : ArcLengthParameterized -> QuadraticSpline2d
fromArcLengthParameterized (ArcLengthParameterized parameterized) =
    parameterized.underlyingSpline


{-| Get the second derivative of a spline (for a quadratic spline, this is a
constant).
-}
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
