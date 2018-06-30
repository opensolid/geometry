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


{-| -}
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


{-| -}
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


{-| -}
startAngle : EllipticalArc2d -> Float
startAngle (Types.EllipticalArc2d arc) =
    arc.startAngle


{-| -}
sweptAngle : EllipticalArc2d -> Float
sweptAngle (Types.EllipticalArc2d arc) =
    arc.sweptAngle


{-| -}
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


{-| -}
pointsAt : List ParameterValue -> EllipticalArc2d -> List Point2d
pointsAt parameterValues arc =
    List.map (pointOn arc) parameterValues


{-| -}
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


{-| -}
firstDerivativesAt : List ParameterValue -> EllipticalArc2d -> List Vector2d
firstDerivativesAt parameterValues arc =
    List.map (firstDerivative arc) parameterValues


{-| -}
type Nondegenerate
    = Curved EllipticalArc2d
    | Horizontal EllipticalArc2d
    | Vertical EllipticalArc2d


{-| -}
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


{-| -}
fromNondegenerate : Nondegenerate -> EllipticalArc2d
fromNondegenerate nondegenerateArc =
    case nondegenerateArc of
        Curved arc ->
            arc

        Horizontal arc ->
            arc

        Vertical arc ->
            arc


{-| -}
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


{-| -}
tangentDirectionsAt : List ParameterValue -> Nondegenerate -> List Direction2d
tangentDirectionsAt parameterValues nondegenerateArc =
    List.map (tangentDirection nondegenerateArc) parameterValues


{-| -}
sample : Nondegenerate -> ParameterValue -> ( Point2d, Direction2d )
sample nondegenerateArc parameterValue =
    ( pointOn (fromNondegenerate nondegenerateArc) parameterValue
    , tangentDirection nondegenerateArc parameterValue
    )


{-| -}
samplesAt : List ParameterValue -> Nondegenerate -> List ( Point2d, Direction2d )
samplesAt parameterValues nondegenerateArc =
    List.map (sample nondegenerateArc) parameterValues


{-| -}
startPoint : EllipticalArc2d -> Point2d
startPoint arc =
    pointOn arc ParameterValue.zero


{-| -}
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


{-| -}
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


{-| -}
scaleAbout : Point2d -> Float -> EllipticalArc2d -> EllipticalArc2d
scaleAbout point scale =
    transformBy (Ellipse2d.scaleAbout point scale)


{-| -}
rotateAround : Point2d -> Float -> EllipticalArc2d -> EllipticalArc2d
rotateAround point angle =
    transformBy (Ellipse2d.rotateAround point angle)


{-| -}
translateBy : Vector2d -> EllipticalArc2d -> EllipticalArc2d
translateBy displacement =
    transformBy (Ellipse2d.translateBy displacement)


{-| -}
translateIn : Direction2d -> Float -> EllipticalArc2d -> EllipticalArc2d
translateIn direction distance arc =
    translateBy (Vector2d.withLength distance direction) arc


{-| -}
mirrorAcross : Axis2d -> EllipticalArc2d -> EllipticalArc2d
mirrorAcross axis =
    transformBy (Ellipse2d.mirrorAcross axis)


{-| -}
relativeTo : Frame2d -> EllipticalArc2d -> EllipticalArc2d
relativeTo frame =
    transformBy (Ellipse2d.relativeTo frame)


{-| -}
placeIn : Frame2d -> EllipticalArc2d -> EllipticalArc2d
placeIn frame =
    transformBy (Ellipse2d.placeIn frame)


{-| -}
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


{-| -}
type ArcLengthParameterized
    = ArcLengthParameterized
        { underlyingArc : EllipticalArc2d
        , parameterization : ArcLengthParameterization
        , nondegenerateArc : Maybe Nondegenerate
        }


{-| -}
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


{-| -}
arcLength : ArcLengthParameterized -> Float
arcLength parameterizedArc =
    arcLengthParameterization parameterizedArc
        |> ArcLengthParameterization.totalArcLength


{-| -}
pointAlong : ArcLengthParameterized -> Float -> Maybe Point2d
pointAlong (ArcLengthParameterized parameterized) distance =
    parameterized.parameterization
        |> ArcLengthParameterization.arcLengthToParameterValue distance
        |> Maybe.map (pointOn parameterized.underlyingArc)


{-| -}
tangentDirectionAlong : ArcLengthParameterized -> Float -> Maybe Direction2d
tangentDirectionAlong (ArcLengthParameterized parameterized) distance =
    case parameterized.nondegenerateArc of
        Just nondegenerateArc ->
            parameterized.parameterization
                |> ArcLengthParameterization.arcLengthToParameterValue distance
                |> Maybe.map (tangentDirection nondegenerateArc)

        Nothing ->
            Nothing


{-| -}
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
