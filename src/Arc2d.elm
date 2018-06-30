module Arc2d
    exposing
        ( Arc2d
        , Nondegenerate
        , centerPoint
        , endPoint
        , firstDerivative
        , firstDerivativesAt
        , from
        , fromNondegenerate
        , mirrorAcross
        , nondegenerate
        , placeIn
        , pointOn
        , pointsAt
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
        , with
        , withRadius
        )

{-| <img src="https://ianmackenzie.github.io/elm-geometry/1.0.0/Arc2d/icon.svg" alt="Arc2d" width="160">

An `Arc2d` is a section of a circle, defined by its center point, start
point and swept angle (the counterclockwise angle from the start point to the
end point). This module includes functionality for

  - Constructing arcs through given points and/or with a given radius
  - Scaling, rotating, translating and mirroring arcs
  - Converting arcs between different coordinate systems

@docs Arc2d


# Constructors

@docs from, with, sweptAround, throughPoints, withRadius


# Properties

@docs centerPoint, radius, startPoint, endPoint, sweptAngle


# Evaluation

@docs pointOn, pointsAt
@docs Nondegenerate, nondegenerate, fromNondegenerate
@docs tangentDirection, tangentDirectionsAt, sample, samplesAt


# Linear approximation

@docs toPolyline


# Transformations

@docs reverse, scaleAbout, rotateAround, translateBy, translateIn, mirrorAcross


# Coordinate conversions

@docs relativeTo, placeIn


# Differentiation

You are unlikely to need to use these functions directly, but they are useful if
you are writing low-level geometric algorithms.

@docs firstDerivative, firstDerivativesAt

-}

import Arc.SweptAngle as SweptAngle exposing (SweptAngle)
import Axis2d exposing (Axis2d)
import Curve.ParameterValue as ParameterValue exposing (ParameterValue)
import Direction2d exposing (Direction2d)
import Frame2d exposing (Frame2d)
import Geometry.Types as Types
import LineSegment2d exposing (LineSegment2d)
import Point2d exposing (Point2d)
import Polyline2d exposing (Polyline2d)
import Vector2d exposing (Vector2d)


{-| -}
type alias Arc2d =
    Types.Arc2d


twoPi : Float
twoPi =
    2 * pi


{-| Construct an arc with from the first given point to the second, with the
given swept angle.

    p1 =
        Point2d.fromCoordinates ( 2, 1 )

    p2 =
        Point2d.fromCoordinates ( 1, 2 )

    arc1 =
        Arc2d.from p1 p2 (degrees 90)

    Arc2d.centerPoint arc1
    --> Point2d.fromCoordinates ( 1, 1 )

    arc2 =
        Arc2d.from p1 p2 (degrees -90)

    Arc2d.centerPoint arc2
    --> Point2d.fromCoordinates ( 2, 2 )

    arc3 =
        Arc2d.from p1 p2 (degrees 180)

    Arc2d.centerPoint arc3
    --> Point2d.fromCoordinates ( 1.5, 1.5 )

    arc4 =
        Arc2d.from p1 p2 (degrees -180)

    Arc2d.centerPoint arc4
    --> Point2d.fromCoordinates ( 1.5, 1.5 )

    arc5 =
        Arc2d.from p1 p2 (degrees 45)

    Arc2d.centerPoint arc5
    --> Point2d.fromCoordinates ( 0.2929, 0.2929 )

-}
from : Point2d -> Point2d -> Float -> Arc2d
from startPoint_ endPoint_ sweptAngle_ =
    let
        displacement =
            Vector2d.from startPoint_ endPoint_
    in
    case Vector2d.lengthAndDirection displacement of
        Just ( distance, direction ) ->
            let
                angleModTwoPi =
                    sweptAngle_ - twoPi * toFloat (floor (sweptAngle_ / twoPi))

                radius_ =
                    distance / (2 * abs (sin (sweptAngle_ / 2)))
            in
            Types.Arc2d
                { startPoint = startPoint_
                , sweptAngle = sweptAngle_
                , xDirection =
                    direction |> Direction2d.rotateBy (-angleModTwoPi / 2)
                , signedLength =
                    if sweptAngle_ == 0.0 then
                        distance
                    else
                        radius_ * sweptAngle_
                }

        Nothing ->
            Types.Arc2d
                { startPoint = startPoint_
                , sweptAngle = sweptAngle_
                , xDirection = Direction2d.x
                , signedLength = 0
                }


{-| Construct an arc with the given center point, radius, start angle and swept
angle:

    arc =
        Arc2d.with
            { centerPoint =
                Point2d.fromCoordinates ( 2, 0 )
            , radius = 1
            , startAngle = degrees 45
            , sweptAngle = degrees -90
            }

    Arc2d.startPoint arc
    --> Point2d.fromCoordinates ( 2.7071, 0.7071 )

    Arc2d.endPoint arc
    --> Point2d.fromCoordinates ( 2.7071, -0.7071 )

-}
with : { centerPoint : Point2d, radius : Float, startAngle : Float, sweptAngle : Float } -> Arc2d
with properties =
    let
        ( x0, y0 ) =
            Point2d.coordinates properties.centerPoint
    in
    Types.Arc2d
        { startPoint =
            Point2d.fromCoordinates
                ( x0 + properties.radius * cos properties.startAngle
                , y0 + properties.radius * sin properties.startAngle
                )
        , sweptAngle = properties.sweptAngle
        , xDirection =
            Direction2d.fromAngle (properties.startAngle + degrees 90)
        , signedLength = abs properties.radius * properties.sweptAngle
        }


{-| Construct an arc by sweeping (rotating) a given start point around a given
center point by a given angle. The center point to sweep around is given first
and the start point to be swept is given last.

    exampleArc =
        Point2d.fromCoordinates ( 3, 1 )
            |> Arc2d.sweptAround
                (Point2d.fromCoordinates ( 1, 1 ))
                (degrees 90)

    Arc2d.endPoint exampleArc
    --> Point2d.fromCoordinates ( 1, 3 )

Note that the 'actual' form of this function is

    arc =
        Arc2d.sweptAround centerPoint sweptAngle startPoint

but it is generally written using the pipe operator `|>` (as in the first
example) to improve readability:

    arc =
        startPoint
            |> Arc2d.sweptAround centerPoint sweptAngle

A positive swept angle means that the arc is formed by rotating the start point
counterclockwise around the center point. A negative swept angle results in
a clockwise arc instead.

-}
sweptAround : Point2d -> Float -> Point2d -> Arc2d
sweptAround centerPoint_ sweptAngle_ startPoint_ =
    case Vector2d.lengthAndDirection (Vector2d.from startPoint_ centerPoint_) of
        Just ( radius_, yDirection ) ->
            Types.Arc2d
                { startPoint = startPoint_
                , xDirection = yDirection |> Direction2d.rotateClockwise
                , sweptAngle = sweptAngle_
                , signedLength = radius_ * sweptAngle_
                }

        Nothing ->
            Types.Arc2d
                { startPoint = startPoint_
                , xDirection = Direction2d.x
                , sweptAngle = sweptAngle_
                , signedLength = 0
                }


{-| Attempt to construct an arc that starts at the first given point, passes
through the second given point and ends at the third given point:

    Arc2d.throughPoints
        Point2d.origin
        (Point2d.fromCoordinates ( 1, 0 ))
        (Point2d.fromCoordinates ( 0, 1 ))
    --> Just
    -->     (Point2d.origin
    -->         |> Arc2d.sweptAround
    -->             (Point2d.fromCoordinates ( 0.5, 0.5 ))
    -->             (degrees 270)
    -->     )

    Arc2d.throughPoints
        (Point2d.fromCoordinates ( 1, 0 ))
        Point2d.origin
        (Point2d.fromCoordinates ( 0, 1 ))
    --> Just
    -->     (Point2d.fromCoordinates ( 1, 0 )
    -->         |> Arc2d.sweptAround
    -->             (Point2d.fromCoordinates ( 0.5, 0.5 ))
    -->             (degrees -180)
    -->     )

If the three points are collinear, returns `Nothing`:

    Arc2d.throughPoints
        Point2d.origin
        (Point2d.fromCoordinates ( 1, 0 ))
        (Point2d.fromCoordinates ( 2, 0 ))
    --> Nothing

    Arc2d.throughPoints
        Point2d.origin
        Point2d.origin
        (Point2d.fromCoordinates ( 1, 0 ))
    --> Nothing

-}
throughPoints : Point2d -> Point2d -> Point2d -> Maybe Arc2d
throughPoints firstPoint secondPoint thirdPoint =
    Point2d.circumcenter firstPoint secondPoint thirdPoint
        |> Maybe.andThen
            (\centerPoint_ ->
                let
                    firstVector =
                        Vector2d.from centerPoint_ firstPoint

                    secondVector =
                        Vector2d.from centerPoint_ secondPoint

                    thirdVector =
                        Vector2d.from centerPoint_ thirdPoint
                in
                Maybe.map3
                    (\firstDirection secondDirection thirdDirection ->
                        let
                            partial =
                                Direction2d.angleFrom firstDirection
                                    secondDirection

                            full =
                                Direction2d.angleFrom firstDirection
                                    thirdDirection

                            sweptAngle_ =
                                if partial >= 0 && full >= partial then
                                    full
                                else if partial <= 0 && full <= partial then
                                    full
                                else if full >= 0 then
                                    full - 2 * pi
                                else
                                    full + 2 * pi
                        in
                        firstPoint |> sweptAround centerPoint_ sweptAngle_
                    )
                    (Vector2d.direction firstVector)
                    (Vector2d.direction secondVector)
                    (Vector2d.direction thirdVector)
            )


{-| Attempt to construct an arc with the given radius between the given start
and end points. Note that this is only possible if the given radius is large
enough! For any given valid radius, start point and end point, there are four
possible results, so the [`SweptAngle`](Arc-SweptAngle) argument is used to
specify which arc to create. For example:

    p1 =
        Point2d.fromCoordinates ( 1, 0 )

    p2 =
        Point2d.fromCoordinates ( 0, 1 )

    Arc2d.withRadius 1 SweptAngle.smallPositive p1 p2
    --> Just
    -->     (Point2d.fromCoordinates ( 1, 0 )
    -->         |> Arc2d.sweptAround Point2d.origin
    -->             (degrees 90)
    -->     )

    Arc2d.withRadius 1 SweptAngle.smallNegative p1 p2
    --> Just
    -->     (Point2d.fromCoordinates ( 1, 0 )
    -->         |> Arc2d.sweptAround
    -->             (Point2d.fromCoordinates ( 1, 1 ))
    -->             (degrees -90)
    -->     )

    Arc2d.withRadius 1 SweptAngle.largePositive p1 p2
    --> Just
    -->     (Point2d.fromCoordinates ( 1, 0 )
    -->         |> Arc2d.sweptAround
    -->             (Point2d.fromCoordinates ( 1, 1 ))
    -->             (degrees 270)
    -->     )

    Arc2d.withRadius 1 SweptAngle.largeNegative p1 p2
    --> Just
    -->     (Point2d.fromCoordinates ( 1, 0 )
    -->         |> Arc2d.sweptAround Point2d.origin
    -->             (degrees -270)
    -->     )

    Arc2d.withRadius 2 SweptAngle.smallPositive p1 p2
    --> Just
    -->     (Point2d.fromCoordinates ( 1, 0 )
    -->         |> Arc2d.sweptAround
    -->             (Point2d.fromCoordinates
    -->                 ( -0.8229, -0.8229 )
    -->             )
    -->             (degrees 41.4096)
    -->     )

If the start and end points are coincident or the distance between them is more
than twice the given radius, returns `Nothing`:

    -- p1 and p2 are too far apart to be connected by an
    -- arc of radius 0.5
    Arc2d.withRadius 0.5 SweptAngle.smallPositive p1 p2
    --> Nothing

Note that this means it is dangerous to use this function to construct 180
degree arcs (half circles), since in this case due to numerical roundoff the
distance between the two given points may appear to be slightly more than twice
the given radius. In this case it is safer to use `Arc2d.from`, such as (for a
counterclockwise arc):

    halfCircle =
        Arc2d.from firstPoint secondPoint (degrees 180)

(Use `degrees -180` for a clockwise arc.)

-}
withRadius : Float -> SweptAngle -> Point2d -> Point2d -> Maybe Arc2d
withRadius radius_ sweptAngle_ startPoint_ endPoint_ =
    let
        chord =
            LineSegment2d.from startPoint_ endPoint_

        squaredRadius =
            radius_ * radius_

        squaredHalfLength =
            LineSegment2d.squaredLength chord / 4
    in
    if squaredRadius >= squaredHalfLength then
        LineSegment2d.perpendicularDirection chord
            |> Maybe.map
                (\offsetDirection ->
                    let
                        offsetMagnitude =
                            sqrt (squaredRadius - squaredHalfLength)

                        offsetDistance =
                            case sweptAngle_ of
                                Types.SmallPositive ->
                                    offsetMagnitude

                                Types.SmallNegative ->
                                    -offsetMagnitude

                                Types.LargeNegative ->
                                    offsetMagnitude

                                Types.LargePositive ->
                                    -offsetMagnitude

                        offset =
                            Vector2d.withLength offsetDistance offsetDirection

                        midpoint =
                            LineSegment2d.midpoint chord

                        centerPoint_ =
                            Point2d.translateBy offset midpoint

                        halfLength =
                            sqrt squaredHalfLength

                        shortAngle =
                            2 * asin (halfLength / radius_)

                        sweptAngleInRadians =
                            case sweptAngle_ of
                                Types.SmallPositive ->
                                    shortAngle

                                Types.SmallNegative ->
                                    -shortAngle

                                Types.LargePositive ->
                                    2 * pi - shortAngle

                                Types.LargeNegative ->
                                    shortAngle - 2 * pi
                    in
                    startPoint_ |> sweptAround centerPoint_ sweptAngleInRadians
                )
    else
        Nothing


{-| Get the center point of an arc.

    Arc2d.centerPoint exampleArc
    --> Point2d.fromCoordinates ( 1, 1 )

-}
centerPoint : Arc2d -> Point2d
centerPoint (Types.Arc2d arc) =
    let
        ( x0, y0 ) =
            Point2d.coordinates arc.startPoint

        ( dx, dy ) =
            Direction2d.components arc.xDirection

        r =
            arc.signedLength / arc.sweptAngle
    in
    Point2d.fromCoordinates ( x0 - r * dy, y0 + r * dx )


{-| Get the radius of an arc.

    Arc2d.radius exampleArc
    --> 2

-}
radius : Arc2d -> Float
radius (Types.Arc2d arc) =
    arc.signedLength / arc.sweptAngle


{-| Get the start point of an arc.

    Arc2d.startPoint exampleArc
    --> Point2d.fromCoordinates ( 3, 1 )

-}
startPoint : Arc2d -> Point2d
startPoint (Types.Arc2d properties) =
    properties.startPoint


{-| Get the end point of an arc.

    Arc2d.endPoint exampleArc
    --> Point2d.fromCoordinates ( 1, 3 )

-}
endPoint : Arc2d -> Point2d
endPoint arc =
    pointOn arc ParameterValue.one


{-| Get the swept angle of an arc in radians.

    Arc2d.sweptAngle exampleArc
    --> 1.5708

The result will be positive for a counterclockwise arc and negative for a
clockwise one.

-}
sweptAngle : Arc2d -> Float
sweptAngle (Types.Arc2d properties) =
    properties.sweptAngle


{-| Get the point along an arc at a given parameter value:

    Arc2d.pointOn exampleArc ParameterValue.zero
    --> Point2d.fromCoordinates ( 3, 1 )

    Arc2d.pointOn exampleArc ParameterValue.half
    --> Point2d.fromCoordinates ( 2.4142, 2.4142 )

    Arc2d.pointOn exampleArc ParameterValue.one
    --> Point2d.fromCoordinates ( 1, 3 )

-}
pointOn : Arc2d -> ParameterValue -> Point2d
pointOn (Types.Arc2d arc) parameterValue =
    let
        ( x0, y0 ) =
            Point2d.coordinates arc.startPoint

        ( dx, dy ) =
            Direction2d.components arc.xDirection

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
        Point2d.fromCoordinates
            ( x0 + distance * dx
            , y0 + distance * dy
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
        Point2d.fromCoordinates
            ( x0 + x * dx - y * dy
            , y0 + x * dy + y * dx
            )


{-| Get points along an arc at a given set of parameter values:

    exampleArc |> Arc2d.pointsAt (ParameterValue.steps 2)
    --> [ Point2d.fromCoordinates ( 3, 1 )
    --> , Point2d.fromCoordinates ( 2.4142, 2.4142 )
    --> , Point2d.fromCoordinates ( 1, 3 )
    --> ]

-}
pointsAt : List ParameterValue -> Arc2d -> List Point2d
pointsAt parameterValues arc =
    List.map (pointOn arc) parameterValues


{-| Get the first derivative of an arc at a given parameter value:

    Arc2d.firstDerivative exampleArc ParameterValue.zero
    --> Vector2d.fromComponents ( 0, 3.1416 )

    Arc2d.firstDerivative exampleArc ParameterValue.half
    --> Vector2d.fromComponents ( -2.2214, 2.2214 )

    Arc2d.firstDerivative exampleArc ParameterValue.one
    --> Vector2d.fromComponents ( -3.1416, 0 )

-}
firstDerivative : Arc2d -> ParameterValue -> Vector2d
firstDerivative (Types.Arc2d arc) =
    let
        startDerivative =
            Vector2d.withLength arc.signedLength arc.xDirection
    in
    \parameterValue ->
        let
            t =
                ParameterValue.value parameterValue
        in
        startDerivative |> Vector2d.rotateBy (t * arc.sweptAngle)


{-| Evaluate the first derivative of an arc at a given set of parameter values:

    exampleArc
        |> Arc2d.firstDerivativesAt
            (ParameterValue.steps 2)
    --> [ Vector2d.fromComponents ( 0, 3.1416 )
    --> , Vector2d.fromComponents ( -2.2214, 2.2214 )
    --> , Vector2d.fromComponents ( -3.1416, 0 )
    --> ]

-}
firstDerivativesAt : List ParameterValue -> Arc2d -> List Vector2d
firstDerivativesAt parameterValues arc =
    List.map (firstDerivative arc) parameterValues


{-| If a curve has zero length (consists of just a single point), then we say
that it is 'degenerate'. Some operations such as computing tangent directions
are not defined on degenerate curves.

A `Nondegenerate` value represents an arc that is definitely not degenerate. It
is used as input to functions such as `Arc2d.tangentDirection` and can be
constructed using `Arc2d.nondegenerate`.

-}
type Nondegenerate
    = Nondegenerate Arc2d


{-| Attempt to construct a nondegenerate arc from a general `Arc2d`. If the arc
is in fact degenerate (consists of a single point), returns an `Err` with that
point.

    Arc2d.nondegenerate exampleArc
    --> Ok nondegenerateExampleArc

-}
nondegenerate : Arc2d -> Result Point2d Nondegenerate
nondegenerate arc =
    let
        (Types.Arc2d properties) =
            arc
    in
    if properties.signedLength == 0 then
        Err (startPoint arc)
    else
        Ok (Nondegenerate arc)


{-| Convert a nondegenerate arc back to a general `Arc2d`.

    Arc2d.fromNondegenerate nondegenerateExampleArc
    --> exampleArc

-}
fromNondegenerate : Nondegenerate -> Arc2d
fromNondegenerate (Nondegenerate arc) =
    arc


{-| Get the tangent direction to a nondegenerate arc at a given parameter
value:

    Arc2d.tangentDirection nondegenerateExampleArc
        ParameterValue.zero
    --> Direction2d.fromAngle (degrees 90)

    Arc2d.tangentDirection nondegenerateExampleArc
        ParameterValue.half
    --> Direction2d.fromAngle (degrees 135)

    Arc2d.tangentDirection nondegenerateExampleArc
        ParameterValue.one
    --> Direction2d.fromAngle (degrees 180)

-}
tangentDirection : Nondegenerate -> ParameterValue -> Direction2d
tangentDirection (Nondegenerate (Types.Arc2d arc)) parameterValue =
    let
        t =
            ParameterValue.value parameterValue
    in
    arc.xDirection |> Direction2d.rotateBy (t * arc.sweptAngle)


{-| Get tangent directions to a nondegenerate arc at a given set of parameter
values:

    nondegenerateExampleArc
        |> Arc2d.tangentDirectionsAt
            (ParameterValue.steps 2)
    --> [ Direction2d.fromAngle (degrees 90)
    --> , Direction2d.fromAngle (degrees 135)
    --> , Direction2d.fromAngle (degrees 180)
    --> ]

-}
tangentDirectionsAt : List ParameterValue -> Nondegenerate -> List Direction2d
tangentDirectionsAt parameterValues nondegenerateArc =
    List.map (tangentDirection nondegenerateArc) parameterValues


{-| Get both the point and tangent direction of a nondegenerate arc at a given
parameter value:

    Arc2d.sample nondegenerateExampleArc
        ParameterValue.zero
    --> ( Point2d.fromCoordinates ( 3, 1 )
    --> , Direction2d.fromAngle (degrees 90)
    --> )

    Arc2d.sample nondegenerateExampleArc
        ParameterValue.half
    --> ( Point2d.fromCoordinates ( 2.4142, 2.4142 )
    --> , Direction2d.fromAngle (degrees 135)
    --> )

    Arc2d.sample nondegenerateExampleArc
        ParameterValue.one
    --> ( Point2d.fromCoordinates ( 1, 3 )
    --> , Direction2d.fromAngle (degrees 180)
    --> )

-}
sample : Nondegenerate -> ParameterValue -> ( Point2d, Direction2d )
sample nondegenerateArc parameterValue =
    ( pointOn (fromNondegenerate nondegenerateArc) parameterValue
    , tangentDirection nondegenerateArc parameterValue
    )


{-| Get points and tangent directions of a nondegenerate arc at a given set of
parameter values:

    nondegenerateExampleArc
        |> Arc2d.samplesAt (ParameterValue.steps 2)
    --> [ ( Point2d.fromCoordinates ( 3, 1 )
    -->   , Direction2d.fromAngle (degrees 90)
    -->   )
    --> , ( Point2d.fromCoordinates ( 2.4142, 2.4142 )
    -->   , Direction2d.fromAngle (degrees 135)
    -->   )
    --> , ( Point2d.fromCoordinates ( 1, 3 )
    -->   , Direction2d.fromAngle (degrees 180)
    -->   )
    --> ]

-}
samplesAt : List ParameterValue -> Nondegenerate -> List ( Point2d, Direction2d )
samplesAt parameterValues nondegenerateArc =
    List.map (sample nondegenerateArc) parameterValues


numApproximationSegments : Float -> Arc2d -> Int
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

    exampleArc |> Arc2d.toPolyline { maxError = 0.1 }
    --> Polyline2d.fromVertices
    -->     [ Point2d.fromCoordinates ( 3, 1 )
    -->     , Point2d.fromCoordinates ( 2.732, 2 )
    -->     , Point2d.fromCoordinates ( 2, 2.732 )
    -->     , Point2d.fromCoordinates ( 1, 3 )
    -->     ]

In this example, every point on the returned polyline will be within 0.1 units
of the original arc.

-}
toPolyline : { maxError : Float } -> Arc2d -> Polyline2d
toPolyline { maxError } arc =
    let
        numSegments =
            numApproximationSegments maxError arc

        points =
            arc |> pointsAt (ParameterValue.steps numSegments)
    in
    Polyline2d.fromVertices points


{-| Reverse the direction of an arc, so that the start point becomes the end
point and vice versa.

    Arc2d.reverse exampleArc
    --> Point2d.fromCoordinates ( 1, 3 )
    -->     |> Arc2d.sweptAround
    -->         (Point2d.fromCoordinates ( 1, 1 ))
    -->         (degrees -90)

-}
reverse : Arc2d -> Arc2d
reverse ((Types.Arc2d arc) as arc_) =
    Types.Arc2d
        { startPoint = endPoint arc_
        , sweptAngle = -arc.sweptAngle
        , signedLength = -arc.signedLength
        , xDirection = arc.xDirection |> Direction2d.rotateBy arc.sweptAngle
        }


{-| Scale an arc about a given point by a given scale.

    point =
        Point2d.fromCoordinates ( 0, 1 )

    Arc2d.scaleAbout point 2 exampleArc
    --> Point2d.fromCoordinates ( 6, 1 )
    -->     |> Arc2d.sweptAround
    -->         (Point2d.fromCoordinates ( 2, 1 ))
    -->         (degrees 90)

-}
scaleAbout : Point2d -> Float -> Arc2d -> Arc2d
scaleAbout point scale (Types.Arc2d arc) =
    Types.Arc2d
        { startPoint = Point2d.scaleAbout point scale arc.startPoint
        , sweptAngle = arc.sweptAngle
        , signedLength = abs scale * arc.signedLength
        , xDirection =
            if scale >= 0 then
                arc.xDirection
            else
                Direction2d.reverse arc.xDirection
        }


{-| Rotate an arc around a given point by a given angle.

    Arc2d.rotateAround Point2d.origin (degrees 90)
    --> Point2d.fromCoordinates ( -1, 3 )
    -->     |> Arc2d.sweptAround
    -->         (Point2d.fromCoordinates ( -1, 1 ))
    -->         (degrees 90)

-}
rotateAround : Point2d -> Float -> Arc2d -> Arc2d
rotateAround point angle =
    let
        rotatePoint =
            Point2d.rotateAround point angle

        rotateDirection =
            Direction2d.rotateBy angle
    in
    \(Types.Arc2d arc) ->
        Types.Arc2d
            { startPoint = rotatePoint arc.startPoint
            , sweptAngle = arc.sweptAngle
            , signedLength = arc.signedLength
            , xDirection = rotateDirection arc.xDirection
            }


{-| Translate an arc by a given displacement.

    displacement =
        Vector2d.fromComponents ( 2, 3 )

    Arc2d.translateBy displacement exampleArc
    --> Point2d.fromCoordinates ( 5, 4 )
    -->     |> Arc2d.sweptAround
    -->         (Point2d.fromCoordinates ( 3, 4 ))
    -->         (degrees 90)

-}
translateBy : Vector2d -> Arc2d -> Arc2d
translateBy displacement (Types.Arc2d arc) =
    Types.Arc2d
        { startPoint = Point2d.translateBy displacement arc.startPoint
        , sweptAngle = arc.sweptAngle
        , signedLength = arc.signedLength
        , xDirection = arc.xDirection
        }


{-| Translate an arc in a given direction by a given distance;

    Arc2d.translateIn direction distance

is equivalent to

    Arc2d.translateBy
        (Vector2d.withLength distance direction)

-}
translateIn : Direction2d -> Float -> Arc2d -> Arc2d
translateIn direction distance arc =
    translateBy (Vector2d.withLength distance direction) arc


{-| Mirror an arc across a given axis.

    Arc2d.mirrorAcross Axis2d.y exampleArc
    --> Point2d.fromCoordinates ( -3, 1 )
    -->     |> Arc2d.sweptAround
    -->         (Point2d.fromCoordinates ( -1, 1 ))
    -->         (degrees -90)

-}
mirrorAcross : Axis2d -> Arc2d -> Arc2d
mirrorAcross axis =
    let
        mirrorPoint =
            Point2d.mirrorAcross axis

        mirrorDirection =
            Direction2d.mirrorAcross axis
    in
    \(Types.Arc2d arc) ->
        Types.Arc2d
            { startPoint = mirrorPoint arc.startPoint
            , sweptAngle = -arc.sweptAngle
            , signedLength = -arc.signedLength
            , xDirection = Direction2d.reverse (mirrorDirection arc.xDirection)
            }


{-| Take an arc defined in global coordinates, and return it expressed in local
coordinates relative to a given reference frame.

    localFrame =
        Frame2d.atPoint (Point2d.fromCoordinates ( 1, 2 ))

    Arc2d.relativeTo localFrame exampleArc
    --> Point2d.fromCoordinates ( 2, -1 )
    -->     |> Arc2d.sweptAround
    -->         (Point2d.fromCoordinates ( 0, -1 ))
    -->         (degrees 90)

-}
relativeTo : Frame2d -> Arc2d -> Arc2d
relativeTo frame (Types.Arc2d arc) =
    if Frame2d.isRightHanded frame then
        Types.Arc2d
            { startPoint = Point2d.relativeTo frame arc.startPoint
            , sweptAngle = arc.sweptAngle
            , signedLength = arc.signedLength
            , xDirection = Direction2d.relativeTo frame arc.xDirection
            }
    else
        Types.Arc2d
            { startPoint = Point2d.relativeTo frame arc.startPoint
            , sweptAngle = -arc.sweptAngle
            , signedLength = -arc.signedLength
            , xDirection =
                Direction2d.reverse
                    (Direction2d.relativeTo frame arc.xDirection)
            }


{-| Take an arc considered to be defined in local coordinates relative to a
given reference frame, and return that arc expressed in global coordinates.

    localFrame =
        Frame2d.atPoint (Point2d.fromCoordinates ( 1, 2 ))

    Arc2d.placeIn localFrame exampleArc
    --> Point2d.fromCoordinates ( 4, 3 )
    -->     |> Arc2d.sweptAround
    -->         (Point2d.fromCoordinates ( 2, 3 ))
    -->         (degrees 90)

-}
placeIn : Frame2d -> Arc2d -> Arc2d
placeIn frame (Types.Arc2d arc) =
    if Frame2d.isRightHanded frame then
        Types.Arc2d
            { startPoint = Point2d.placeIn frame arc.startPoint
            , sweptAngle = arc.sweptAngle
            , signedLength = arc.signedLength
            , xDirection = Direction2d.placeIn frame arc.xDirection
            }
    else
        Types.Arc2d
            { startPoint = Point2d.placeIn frame arc.startPoint
            , sweptAngle = -arc.sweptAngle
            , signedLength = -arc.signedLength
            , xDirection =
                Direction2d.reverse (Direction2d.placeIn frame arc.xDirection)
            }
