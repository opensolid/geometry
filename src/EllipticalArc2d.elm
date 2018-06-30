module EllipticalArc2d
    exposing
        ( ArcLengthParameterized
        , EllipticalArc2d
        , Nondegenerate
        , arcLength
        , arcLengthParameterization
        , arcLengthParameterized
        , axes
        , centerPoint
        , endPoint
        , firstDerivative
        , firstDerivativesAt
        , fromArcLengthParameterized
        , fromEndpoints
        , fromNondegenerate
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
        , startAngle
        , startPoint
        , sweptAngle
        , tangentDirection
        , tangentDirectionAlong
        , tangentDirectionsAt
        , translateBy
        , translateIn
        , with
        , xAxis
        , xDirection
        , xRadius
        , yAxis
        , yDirection
        , yRadius
        )

{-| <img src="https://ianmackenzie.github.io/elm-geometry/1.0.0/EllipticalArc2d/icon.svg" alt="EllipticalArc2d" width="160">

An `EllipticalArc2d` is a section of an `Ellipse2d` with a start and end point.
This module includes functionality for

  - Constructing an elliptical arc from its center or end points
  - Scaling, rotating, translating and mirroring elliptical arcs
  - Evaluating points and derivative vectors along elliptical arcs
  - Forming arc length parameterizations of elliptical arcs

The `startAngle` and `sweptAngle` values referred to below are not actually
proper angles but instead refer to values of the [ellipse parameter](https://en.wikipedia.org/wiki/Ellipse#Parametric_representation).
However, in simple cases you don't need to worry about the difference - if
`startAngle` and `sweptAngle` are both multiples of 90 degrees, then you can
treat them as actual angles and everything will behave as you expect.

@docs EllipticalArc2d


# Constructors

@docs with, fromEndpoints


# Properties

@docs startAngle, sweptAngle, startPoint, endPoint

All remaining properties of elliptical arcs are actually just properties of the
underlying ellipse; check out the [Ellipse2d](Ellipse2d) module for details.

@docs centerPoint, axes, xAxis, yAxis, xDirection, yDirection, xRadius, yRadius


# Evaluation

@docs pointOn, pointsAt
@docs Nondegenerate, nondegenerate, fromNondegenerate
@docs tangentDirection, tangentDirectionsAt, sample, samplesAt


# Transformations

@docs reverse, scaleAbout, rotateAround, translateBy, translateIn, mirrorAcross


# Coordinate conversions

@docs relativeTo, placeIn


# Arc length parameterization

@docs ArcLengthParameterized, arcLengthParameterized, arcLength, pointAlong, tangentDirectionAlong, sampleAlong


## Low level

An `ArcLengthParameterized` value is a combination of an
[`ArcLengthParameterization`](Geometry-ArcLengthParameterization) and an
underlying `EllipticalArc2d`. If you need to do something fancy, you can extract
these two values separately.

@docs arcLengthParameterization, fromArcLengthParameterized


# Differentiation

You are unlikely to need to use these functions directly, but they are useful if
you are writing low-level geometric algorithms.

@docs firstDerivative, firstDerivativesAt, maxSecondDerivativeMagnitude

-}

import Arc.SweptAngle as SweptAngle exposing (SweptAngle)
import Axis2d exposing (Axis2d)
import Curve.ArcLengthParameterization as ArcLengthParameterization exposing (ArcLengthParameterization)
import Curve.ParameterValue as ParameterValue exposing (ParameterValue)
import Direction2d exposing (Direction2d)
import Ellipse2d exposing (Ellipse2d)
import Frame2d exposing (Frame2d)
import Geometry.Types as Types
import Interval
import Point2d exposing (Point2d)
import Vector2d exposing (Vector2d)


{-| -}
type alias EllipticalArc2d =
    Types.EllipticalArc2d


{-| Construct an elliptical arc from its center point, X direction, X and Y
radii, start angle and swept angle. If you pass a negative radius, the absolute
value will be used.

For example, to construct a simple 90 degree elliptical arc, you might use

    exampleArc =
        EllipticalArc2d.with
            { centerPoint = Point2d.origin
            , xDirection = Direction2d.x
            , xRadius = 2
            , yRadius = 1
            , startAngle = 0
            , sweptAngle = degrees 90
            }

![90 degree elliptical arc](https://ianmackenzie.github.io/elm-geometry/1.0.0/EllipticalArc2d/with1.svg)

To make an inclined 180 degree elliptical arc, you might use

    EllipticalArc2d.with
        { centerPoint = Point2d.origin
        , xDirection = Direction2d.fromAngle (degrees 30)
        , xRadius = 2
        , yRadius = 1
        , startAngle = degrees -90
        , sweptAngle = degrees 180
        }

![180 degree inclined elliptical arc](https://ianmackenzie.github.io/elm-geometry/1.0.0/EllipticalArc2d/with2.svg)

-}
with : { centerPoint : Point2d, xDirection : Direction2d, xRadius : Float, yRadius : Float, startAngle : Float, sweptAngle : Float } -> EllipticalArc2d
with properties =
    Types.EllipticalArc2d
        { ellipse =
            Ellipse2d.with
                { centerPoint = properties.centerPoint
                , xDirection = properties.xDirection
                , xRadius = properties.xRadius
                , yRadius = properties.yRadius
                }
        , startAngle = properties.startAngle
        , sweptAngle = properties.sweptAngle
        }


{-| Attempt to construct an elliptical arc from its endpoints, X direction, and
X and Y radii. For any given valid set of these inputs, there are four possible
solutions, so you also need to specify which of the four solutions you want -
whether the swept angle of the arc should be less than or greater than 180
degrees, and whether the swept angle should be positive (counterclockwise) or
negative (clockwise).

The example below is interactive; try dragging either endpoint or the tip of the
X direction (or the center point to move the whole arc), clicking on the X or Y
radial lines and then scrolling to changet that radius, or clicking/tapping on
the various dashed arcs to switch what kind of swept angle to use.

<iframe src="https://ianmackenzie.github.io/elm-geometry/1.0.0/EllipticalArc2d/fromEndpoints.html" style="width: 500px; height: 400px" scrolling=no frameborder=0>
`https://ianmackenzie.github.io/elm-geometry/1.0.0/EllipticalArc2d/fromEndpoints.html`
</iframe>

This function will return `Nothing` if no solution can found. Typically this
means that the two endpoints are too far apart, but could also mean that one of
the specified radii was negative or zero, or the two given points were
coincident.

The behavior of this function is very close to [the SVG spec](https://www.w3.org/TR/SVG/implnote.html#ArcImplementationNotes),
but when 'out of range' parameters are given this function will simply return
`Nothing` instead of attempting to degrade gracefully (for example, by
increasing X and Y radius slightly if the given endpoints are slightly too far
apart). Note that this means this function is dangerous to use for 180 degree
arcs, since then slight numerical roundoff can mean the difference between a
solution being found and not - for 180 degree arcs it is safer to use
`EllipticalArc2d.with` instead.

-}
fromEndpoints : { startPoint : Point2d, endPoint : Point2d, xRadius : Float, yRadius : Float, xDirection : Direction2d, sweptAngle : SweptAngle } -> Maybe EllipticalArc2d
fromEndpoints arguments =
    if arguments.xRadius > 0 && arguments.yRadius > 0 then
        let
            temporaryFrame =
                Frame2d.withXDirection arguments.xDirection
                    (arguments.startPoint
                        |> Point2d.translateIn arguments.xDirection
                            -arguments.xRadius
                    )

            ( x2Ellipse, y2Ellipse ) =
                arguments.endPoint
                    |> Point2d.relativeTo temporaryFrame
                    |> Point2d.coordinates

            x2 =
                x2Ellipse / arguments.xRadius

            y2 =
                y2Ellipse / arguments.yRadius

            cx2 =
                x2 - 1

            cy2 =
                y2

            d =
                sqrt (cx2 * cx2 + cy2 * cy2) / 2
        in
        if d > 0 && d < 1 then
            let
                midAngle =
                    atan2 -cy2 -cx2

                offsetAngle =
                    acos d

                ( startAngle_, sweptAngleInRadians ) =
                    case arguments.sweptAngle of
                        Types.SmallPositive ->
                            ( midAngle + offsetAngle
                            , pi - 2 * offsetAngle
                            )

                        Types.SmallNegative ->
                            ( midAngle - offsetAngle
                            , -pi + 2 * offsetAngle
                            )

                        Types.LargePositive ->
                            ( midAngle - offsetAngle
                            , pi + 2 * offsetAngle
                            )

                        Types.LargeNegative ->
                            ( midAngle + offsetAngle
                            , -pi - 2 * offsetAngle
                            )

                centerPoint_ =
                    Point2d.fromCoordinatesIn temporaryFrame
                        ( arguments.xRadius * (1 - cos startAngle_)
                        , -arguments.yRadius * sin startAngle_
                        )
            in
            Just <|
                with
                    { centerPoint = centerPoint_
                    , xDirection = arguments.xDirection
                    , xRadius = arguments.xRadius
                    , yRadius = arguments.yRadius
                    , startAngle =
                        if startAngle_ > pi then
                            startAngle_ - 2 * pi
                        else if startAngle_ < -pi then
                            startAngle_ + 2 * pi
                        else
                            startAngle_
                    , sweptAngle = sweptAngleInRadians
                    }
        else
            Nothing
    else
        Nothing


{-| -}
centerPoint : EllipticalArc2d -> Point2d
centerPoint (Types.EllipticalArc2d arc) =
    Ellipse2d.centerPoint arc.ellipse


{-| -}
axes : EllipticalArc2d -> Frame2d
axes (Types.EllipticalArc2d arc) =
    Ellipse2d.axes arc.ellipse


{-| -}
xAxis : EllipticalArc2d -> Axis2d
xAxis (Types.EllipticalArc2d arc) =
    Ellipse2d.xAxis arc.ellipse


{-| -}
yAxis : EllipticalArc2d -> Axis2d
yAxis (Types.EllipticalArc2d arc) =
    Ellipse2d.yAxis arc.ellipse


{-| -}
xRadius : EllipticalArc2d -> Float
xRadius (Types.EllipticalArc2d arc) =
    Ellipse2d.xRadius arc.ellipse


{-| -}
yRadius : EllipticalArc2d -> Float
yRadius (Types.EllipticalArc2d arc) =
    Ellipse2d.yRadius arc.ellipse


{-| The start angle of an elliptical arc is the value of the [ellipse parameter](https://en.wikipedia.org/wiki/Ellipse#Parametric_representation)
at the start point of the arc.

    EllipticalArc2d.startAngle exampleArc
    --> 0

-}
startAngle : EllipticalArc2d -> Float
startAngle (Types.EllipticalArc2d arc) =
    arc.startAngle


{-| The swept angle of an elliptical arc is the difference between values of the
[ellipse parameter](https://en.wikipedia.org/wiki/Ellipse#Parametric_representation)
from the start point to the end point of the arc.

    EllipticalArc2d.sweptAngle exampleArc
    --> degrees 90

-}
sweptAngle : EllipticalArc2d -> Float
sweptAngle (Types.EllipticalArc2d arc) =
    arc.sweptAngle


{-| Get the point along an elliptical arc at a given parameter value:

    EllipticalArc2d.pointOn exampleArc ParameterValue.zero
    --> Point2d.fromCoordinates ( 2, 0 )

    EllipticalArc2d.pointOn exampleArc ParameterValue.half
    --> Point2d.fromCoordinates ( 1.4142, 0.7071 )

    EllipticalArc2d.pointOn exampleArc ParameterValue.one
    --> Point2d.fromCoordinates ( 0, 1 )

-}
pointOn : EllipticalArc2d -> ParameterValue -> Point2d
pointOn arc parameterValue =
    let
        t =
            ParameterValue.value parameterValue

        theta =
            startAngle arc + t * sweptAngle arc
    in
    Point2d.fromCoordinatesIn (axes arc)
        ( xRadius arc * cos theta
        , yRadius arc * sin theta
        )


{-| Get points along an elliptical arc at a given set of parameter values:

    exampleArc
        |> EllipticalArc2d.pointsAt
            (ParameterValue.steps 2)
    --> [ Point2d.fromCoordinates ( 2, 0 )
    --> , Point2d.fromCoordinates ( 1.4142, 0.7071 )
    --> , Point2d.fromCoordinates ( 0, 1 )
    --> ]

-}
pointsAt : List ParameterValue -> EllipticalArc2d -> List Point2d
pointsAt parameterValues arc =
    List.map (pointOn arc) parameterValues


{-| Get the first derivative of an elliptical arc at a given parameter value:

    EllipticalArc2d.firstDerivative exampleArc
        ParameterValue.zero
    --> Vector2d.fromComponents ( 0, 1.5708 )

    EllipticalArc2d.firstDerivative exampleArc
        ParameterValue.half
    --> Vector2d.fromComponents ( -2.2214, 1.1107 )

    EllipticalArc2d.firstDerivative exampleArc
        ParameterValue.one
    --> Vector2d.fromComponents ( -3.1416, 0 )

-}
firstDerivative : EllipticalArc2d -> ParameterValue -> Vector2d
firstDerivative arc parameterValue =
    let
        t =
            ParameterValue.value parameterValue

        deltaTheta =
            sweptAngle arc

        theta =
            startAngle arc + t * deltaTheta
    in
    Vector2d.placeIn (axes arc) <|
        Vector2d.fromComponents
            ( -(xRadius arc) * deltaTheta * sin theta
            , yRadius arc * deltaTheta * cos theta
            )


{-| Evaluate the first derivative of an elliptical arc at a given set of
parameter values:

    exampleArc
        |> EllipticalArc2d.firstDerivativesAt
            (ParameterValue.steps 2)
    --> [ Vector2d.fromComponents ( 0, 1.5708 )
    --> , Vector2d.fromComponents ( -2.2214, 1.1107 )
    --> , Vector2d.fromComponents ( -3.1416, 0 )
    --> ]

-}
firstDerivativesAt : List ParameterValue -> EllipticalArc2d -> List Vector2d
firstDerivativesAt parameterValues arc =
    List.map (firstDerivative arc) parameterValues


{-| If a curve has zero length (consists of just a single point), then we say
that it is 'degenerate'. Some operations such as computing tangent directions
are not defined on degenerate curves.

A `Nondegenerate` value represents an arc that is definitely not degenerate. It
is used as input to functions such as `EllipticalArc2d.tangentDirection` and can
be constructed using `EllipticalArc2d.nondegenerate`.

-}
type Nondegenerate
    = Curved EllipticalArc2d
    | Horizontal EllipticalArc2d
    | Vertical EllipticalArc2d


{-| Attempt to construct a nondegenerate elliptical arc from a general
`EllipticalArc2d`. If the arc is in fact degenerate (consists of a single
point), returns an `Err` with that point.

    EllipticalArc2d.nondegenerate exampleArc
    --> Ok nondegenerateExampleArc

-}
nondegenerate : EllipticalArc2d -> Result Point2d Nondegenerate
nondegenerate arc =
    let
        rx =
            xRadius arc

        ry =
            yRadius arc
    in
    if sweptAngle arc == 0 then
        Err (startPoint arc)
    else if rx == 0 && ry == 0 then
        Err (startPoint arc)
    else if rx == 0 then
        Ok (Vertical arc)
    else if ry == 0 then
        Ok (Horizontal arc)
    else
        Ok (Curved arc)


{-| Convert a nondegenerate elliptical arc back to a general `EllipticalArc2d`.

    EllipticalArc2d.fromNondegenerate
        nondegenerateExampleArc
    --> exampleArc

-}
fromNondegenerate : Nondegenerate -> EllipticalArc2d
fromNondegenerate nondegenerateArc =
    case nondegenerateArc of
        Curved arc ->
            arc

        Horizontal arc ->
            arc

        Vertical arc ->
            arc


{-| Get the tangent direction to a nondegenerate elliptical arc at a given
parameter value:

    EllipticalArc2d.tangentDirection nondegenerateExampleArc
        ParameterValue.zero
    --> Direction2d.fromAngle (degrees 90)

    EllipticalArc2d.tangentDirection nondegenerateExampleArc
        ParameterValue.half
    --> Direction2d.fromAngle (degrees 153.4)

    EllipticalArc2d.tangentDirection nondegenerateExampleArc
        ParameterValue.one
    --> Direction2d.fromAngle (degrees 180)

-}
tangentDirection : Nondegenerate -> ParameterValue -> Direction2d
tangentDirection nondegenerateArc parameterValue =
    let
        arc =
            fromNondegenerate nondegenerateArc

        t =
            ParameterValue.value parameterValue

        angle =
            startAngle arc + t * sweptAngle arc
    in
    case nondegenerateArc of
        Curved curvedArc ->
            let
                sinAngle =
                    sin angle

                cosAngle =
                    cos angle

                vx =
                    -(xRadius curvedArc) * sinAngle

                vy =
                    yRadius curvedArc * cosAngle
            in
            -- Since xRadius_ and yRadius_ are both non-zero and at least one of
            -- sinAngle or cosAngle must be non-zero, one of vx or vy will
            -- always be non-zero and therefore normalizing the vector (vx, vy)
            -- is safe
            Vector2d.fromComponents ( vx, vy )
                |> Vector2d.normalize
                |> Vector2d.components
                |> Direction2d.unsafe
                |> Direction2d.placeIn (axes arc)

        Vertical verticalArc ->
            if cos angle >= 0 then
                yDirection verticalArc
            else
                Direction2d.reverse (yDirection verticalArc)

        Horizontal horizontalArc ->
            if sin angle >= 0 then
                Direction2d.reverse (xDirection horizontalArc)
            else
                xDirection horizontalArc


{-| Get tangent directions to a nondegenerate elliptical arc at a given set of
parameter values:

    nondegenerateExampleArc
        |> EllipticalArc2d.tangentDirectionsAt
            (ParameterValue.steps 2)
    --> [ Direction2d.fromAngle (degrees 90)
    --> , Direction2d.fromAngle (degrees 153.4)
    --> , Direction2d.fromAngle (degrees 180)
    --> ]

-}
tangentDirectionsAt : List ParameterValue -> Nondegenerate -> List Direction2d
tangentDirectionsAt parameterValues nondegenerateArc =
    List.map (tangentDirection nondegenerateArc) parameterValues


{-| Get both the point and tangent direction of a nondegenerate elliptical arc
at a given parameter value:

    EllipticalArc2d.sample nondegenerateExampleArc
        ParameterValue.zero
    --> ( Point2d.fromCoordinates ( 2, 0 )
    --> , Direction2d.fromAngle (degrees 90)
    --> )

    EllipticalArc2d.sample nondegenerateExampleArc
        ParameterValue.half
    --> ( Point2d.fromCoordinates ( 1.4142, 0.7071 )
    --> , Direction2d.fromAngle (degrees 153.4)
    --> )

    EllipticalArc2d.sample nondegenerateExampleArc
        ParameterValue.one
    --> ( Point2d.fromCoordinates ( 0, 1 )
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
        |> EllipticalArc2d.samplesAt
            (ParameterValue.steps 2)
    --> [ ( Point2d.fromCoordinates ( 2, 0 )
    -->   , Direction2d.fromAngle (degrees 90)
    -->   )
    --> , ( Point2d.fromCoordinates ( 1.4142, 0.7071 )
    -->   , Direction2d.fromAngle (degrees 153.4)
    -->   )
    --> , ( Point2d.fromCoordinates ( 0, 1 )
    -->   , Direction2d.fromAngle (degrees 180)
    -->   )
    --> ]

-}
samplesAt : List ParameterValue -> Nondegenerate -> List ( Point2d, Direction2d )
samplesAt parameterValues nondegenerateArc =
    List.map (sample nondegenerateArc) parameterValues


{-| Get the start point of an elliptical arc.

    EllipticalArc2d.startPoint exampleArc
    --> Point2d.fromCoordinates ( 2, 0 )

-}
startPoint : EllipticalArc2d -> Point2d
startPoint arc =
    pointOn arc ParameterValue.zero


{-| Get the end point of an elliptical arc.

    EllipticalArc2d.endPoint exampleArc
    --> Point2d.fromCoordinates ( 0, 1 )

-}
endPoint : EllipticalArc2d -> Point2d
endPoint arc =
    pointOn arc ParameterValue.one


{-| -}
xDirection : EllipticalArc2d -> Direction2d
xDirection arc =
    Frame2d.xDirection (axes arc)


{-| -}
yDirection : EllipticalArc2d -> Direction2d
yDirection arc =
    Frame2d.yDirection (axes arc)


{-| Reverse the direction of an elliptical arc, so that the start point becomes
the end point and vice versa. Does not change the shape of the arc or any
properties of the underlying ellipse.

    EllipticalArc2d.reverse exampleArc
    --> EllipticalArc2d.with
    -->     { centerPoint = Point2d.origin
    -->     , xDirection = Direction2d.x
    -->     , xRadius = 2
    -->     , yRadius = 1
    -->     , startAngle = degrees 90
    -->     , sweptAngle = degrees -90
    -->     }

-}
reverse : EllipticalArc2d -> EllipticalArc2d
reverse (Types.EllipticalArc2d properties) =
    Types.EllipticalArc2d
        { properties
            | startAngle = properties.startAngle + properties.sweptAngle
            , sweptAngle = -properties.sweptAngle
        }


transformBy : (Ellipse2d -> Ellipse2d) -> EllipticalArc2d -> EllipticalArc2d
transformBy ellipseTransformation (Types.EllipticalArc2d properties) =
    Types.EllipticalArc2d
        { properties | ellipse = ellipseTransformation properties.ellipse }


{-| Scale an elliptical arc about a given point by a given scale.

    exampleArc
        |> EllipticalArc2d.scaleAbout Point2d.origin 3
    --> EllipticalArc2d.with
    -->     { centerPoint = Point2d.origin
    -->     , xDirection = Direction2d.x
    -->     , xRadius = 6
    -->     , yRadius = 3
    -->     , startAngle = 0
    -->     , sweptAngle = degrees 90
    -->     }

-}
scaleAbout : Point2d -> Float -> EllipticalArc2d -> EllipticalArc2d
scaleAbout point scale =
    transformBy (Ellipse2d.scaleAbout point scale)


{-| Rotate an elliptical arc around a given point by a given angle (in radians).

    exampleArc
        |> EllipticalArc2d.rotateAround Point2d.origin
            (degrees 180)
    --> EllipticalArc2d.with
    -->     { centerPoint = Point2d.origin
    -->     , xDirection = Direction2d.negativeX
    -->     , xRadius = 2
    -->     , yRadius = 1
    -->     , startAngle = 0
    -->     , sweptAngle = degrees 90
    -->     }

-}
rotateAround : Point2d -> Float -> EllipticalArc2d -> EllipticalArc2d
rotateAround point angle =
    transformBy (Ellipse2d.rotateAround point angle)


{-| Translate an elliptical arc by a given displacement.

    exampleArc
        |> EllipticalArc2d.translateBy
            (Vector2d.fromComponents ( 2, 3 ))
    --> EllipticalArc2d.with
    -->     { centerPoint =
    -->         Point2d.fromCoordinates ( 2, 3 )
    -->     , xDirection = Direction2d.x
    -->     , xRadius = 2
    -->     , yRadius = 1
    -->     , startAngle = 0
    -->     , sweptAngle = degrees 90
    -->     }

-}
translateBy : Vector2d -> EllipticalArc2d -> EllipticalArc2d
translateBy displacement =
    transformBy (Ellipse2d.translateBy displacement)


{-| Translate an elliptical arc in a given direction by a given distance;

    EllipticalArc2d.translateIn direction distance

is equivalent to

    EllipticalArc2d.translateBy
        (Vector2d.withLength distance direction)

-}
translateIn : Direction2d -> Float -> EllipticalArc2d -> EllipticalArc2d
translateIn direction distance arc =
    translateBy (Vector2d.withLength distance direction) arc


{-| Mirror an elliptical arc across a given axis.

    mirroredArc =
        exampleArc
            |> EllipticalArc2d.mirrorAcross Axis2d.y

    EllipticalArc2d.startPoint mirroredArc
    --> Point2d.fromCoordinates ( -2, 0 )

    EllipticalArc2d.endPoint mirroredArc
    --> Point2d.fromCoordinates ( 0, 1 )

-}
mirrorAcross : Axis2d -> EllipticalArc2d -> EllipticalArc2d
mirrorAcross axis =
    transformBy (Ellipse2d.mirrorAcross axis)


{-| Take an elliptical arc defined in global coordinates, and return it expressed in
local coordinates relative to a given reference frame.

    localFrame =
        Frame2d.atPoint (Point2d.fromCoordinates ( 1, 2 ))

    EllipticalArc2d.relativeTo localFrame exampleArc
    --> EllipticalArc2d.with
    -->     { centerPoint =
    -->         Point2d.fromCoordinates ( -1, -2 )
    -->     , xDirection = Direction2d.x
    -->     , xRadius = 2
    -->     , yRadius = 1
    -->     , startAngle = 0
    -->     , sweptAngle = degrees 90
    -->     }

-}
relativeTo : Frame2d -> EllipticalArc2d -> EllipticalArc2d
relativeTo frame =
    transformBy (Ellipse2d.relativeTo frame)


{-| Take an elliptical arc considered to be defined in local coordinates
relative to a given reference frame, and return that arc expressed in global
coordinates.

    localFrame =
        Frame2d.atPoint (Point2d.fromCoordinates ( 1, 2 ))

    EllipticalArc2d.relativeTo localFrame exampleArc
    --> EllipticalArc2d.with
    -->     { centerPoint =
    -->         Point2d.fromCoordinates ( 1, 2 )
    -->     , xDirection = Direction2d.x
    -->     , xRadius = 2
    -->     , yRadius = 1
    -->     , startAngle = 0
    -->     , sweptAngle = degrees 90
    -->     }

-}
placeIn : Frame2d -> EllipticalArc2d -> EllipticalArc2d
placeIn frame =
    transformBy (Ellipse2d.placeIn frame)


{-| Find a conservative upper bound on the magnitude of the second derivative of
an elliptical arc. This can be useful when determining error bounds for various
kinds of linear approximations.

    exampleArc
        |> EllipticalArc2d.maxSecondDerivativeMagnitude
    --> 4.935

-}
maxSecondDerivativeMagnitude : EllipticalArc2d -> Float
maxSecondDerivativeMagnitude arc =
    let
        theta0 =
            startAngle arc

        dTheta =
            sweptAngle arc

        theta1 =
            theta0 + dTheta

        dThetaSquared =
            dTheta * dTheta

        rx =
            xRadius arc

        ry =
            yRadius arc

        kx =
            dThetaSquared * rx

        ky =
            dThetaSquared * ry

        thetaInterval =
            Interval.from theta0 theta1

        sinThetaInterval =
            Interval.sin thetaInterval

        includeKx =
            Interval.contains 0 sinThetaInterval

        includeKy =
            (Interval.maxValue sinThetaInterval == 1)
                || (Interval.minValue sinThetaInterval == -1)
    in
    if (kx >= ky) && includeKx then
        -- kx is the global max and is included in the arc
        kx
    else if (ky >= kx) && includeKy then
        -- ky is the global max and is included in the arc
        ky
    else
        -- global max is not included in the arc, so max must be at an endpoint
        let
            rxSquared =
                rx * rx

            rySquared =
                ry * ry

            cosTheta0 =
                cos theta0

            sinTheta0 =
                sin theta0

            cosTheta1 =
                cos theta1

            sinTheta1 =
                sin theta1

            d0 =
                (rxSquared * cosTheta0 * cosTheta0)
                    + (rySquared * sinTheta0 * sinTheta0)

            d1 =
                (rxSquared * cosTheta1 * cosTheta1)
                    + (rySquared * sinTheta1 * sinTheta1)
        in
        dThetaSquared * sqrt (max d0 d1)


derivativeMagnitude : EllipticalArc2d -> ParameterValue -> Float
derivativeMagnitude arc =
    let
        rx =
            xRadius arc

        ry =
            yRadius arc

        theta0 =
            startAngle arc

        dTheta =
            sweptAngle arc

        absDTheta =
            abs dTheta
    in
    \parameterValue ->
        let
            t =
                ParameterValue.value parameterValue

            theta =
                theta0 + t * dTheta

            dx =
                rx * sin theta

            dy =
                ry * cos theta
        in
        absDTheta * sqrt (dx * dx + dy * dy)


{-| An elliptical arc that has been parameterized by arc length.
-}
type ArcLengthParameterized
    = ArcLengthParameterized
        { underlyingArc : EllipticalArc2d
        , parameterization : ArcLengthParameterization
        , nondegenerateArc : Maybe Nondegenerate
        }


{-| Build an arc length parameterization of the given elliptical arc, with a
given accuracy. Generally speaking, all operations on the resulting
`ArcLengthParameterized` value will be accurate to within the specified maximum
error.

    parameterizedArc =
        exampleArc
            |> EllipticalArc2d.arcLengthParameterized
                { maxError = 1.0e-4 }

-}
arcLengthParameterized : { maxError : Float } -> EllipticalArc2d -> ArcLengthParameterized
arcLengthParameterized { maxError } arc =
    let
        parameterization =
            ArcLengthParameterization.build
                { maxError = maxError
                , derivativeMagnitude = derivativeMagnitude arc
                , maxSecondDerivativeMagnitude =
                    maxSecondDerivativeMagnitude arc
                }
    in
    ArcLengthParameterized
        { underlyingArc = arc
        , parameterization = parameterization
        , nondegenerateArc = Result.toMaybe (nondegenerate arc)
        }


{-| Find the total arc length of an elliptical arc. This will be accurate to
within the tolerance given when calling `arcLengthParameterized`.

    arcLength : Float
    arcLength =
        EllipticalArc2d.arcLength parameterizedArc

    arcLength
    --> 2.4221

-}
arcLength : ArcLengthParameterized -> Float
arcLength parameterizedArc =
    arcLengthParameterization parameterizedArc
        |> ArcLengthParameterization.totalArcLength


{-| Try to get the point along an elliptical arc at a given arc length. For
example, to get the true midpoint of `exampleArc`:

    EllipticalArc2d.pointAlong parameterizedArc
        (arcLength / 2)
    --> Just (Point2d.fromCoordinates ( 1.1889, 0.8041 ))

Note that this is not the same as evaulating at a parameter value of 0.5:

    EllipticalArc2d.pointOn exampleArc
        ParameterValue.half
    --> Point2d.fromCoordinates ( 1.4142, 0.7071 )

If the given arc length is less than zero or greater than the arc length of the
arc, returns `Nothing`.

-}
pointAlong : ArcLengthParameterized -> Float -> Maybe Point2d
pointAlong (ArcLengthParameterized parameterized) distance =
    parameterized.parameterization
        |> ArcLengthParameterization.arcLengthToParameterValue distance
        |> Maybe.map (pointOn parameterized.underlyingArc)


{-| Try to get the tangent direction along an elliptical arc at a given arc
length. To get the tangent direction at the midpoint of `exampleArc`:

    EllipticalArc2d.tangentDirectionAlong parameterizedArc
        (arcLength / 2)
    --> Just (Direction2d.fromAngle (degrees 159.7))

If the given arc length is less than zero or greater than the arc length of the
elliptical arc (or if the elliptical arc is degenerate), returns `Nothing`.

-}
tangentDirectionAlong : ArcLengthParameterized -> Float -> Maybe Direction2d
tangentDirectionAlong (ArcLengthParameterized parameterized) distance =
    case parameterized.nondegenerateArc of
        Just nondegenerateArc ->
            parameterized.parameterization
                |> ArcLengthParameterization.arcLengthToParameterValue distance
                |> Maybe.map (tangentDirection nondegenerateArc)

        Nothing ->
            Nothing


{-| Try to get the point and tangent direction along an elliptical arc at a
given arc length. To get the point and tangent direction at the midpoint of
`exampleArc`:

    EllipticalArc2d.sampleAlong parameterizedArc
        (arcLength / 2)
    --> Just
    -->     ( Point2d.fromCoordinates ( 1.1889, 0.8041 )
    -->     , Direction2d.fromAngle (degrees 159.7)
    -->     )

If the given arc length is less than zero or greater than the arc length of the
spline (or if the spline is degenerate), returns `Nothing`.

-}
sampleAlong : ArcLengthParameterized -> Float -> Maybe ( Point2d, Direction2d )
sampleAlong (ArcLengthParameterized parameterized) distance =
    case parameterized.nondegenerateArc of
        Just nondegenerateArc ->
            parameterized.parameterization
                |> ArcLengthParameterization.arcLengthToParameterValue distance
                |> Maybe.map (sample nondegenerateArc)

        Nothing ->
            Nothing


{-| -}
arcLengthParameterization : ArcLengthParameterized -> ArcLengthParameterization
arcLengthParameterization (ArcLengthParameterized parameterized) =
    parameterized.parameterization


{-| -}
fromArcLengthParameterized : ArcLengthParameterized -> EllipticalArc2d
fromArcLengthParameterized (ArcLengthParameterized parameterized) =
    parameterized.underlyingArc
