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


{-| -}
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


{-| -}
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


{-| -}
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


{-| -}
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


{-| -}
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


{-| -}
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


{-| -}
radius : Arc2d -> Float
radius (Types.Arc2d arc) =
    arc.signedLength / arc.sweptAngle


{-| -}
startPoint : Arc2d -> Point2d
startPoint (Types.Arc2d properties) =
    properties.startPoint


{-| -}
endPoint : Arc2d -> Point2d
endPoint arc =
    pointOn arc ParameterValue.one


{-| -}
sweptAngle : Arc2d -> Float
sweptAngle (Types.Arc2d properties) =
    properties.sweptAngle


{-| -}
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


{-| -}
pointsAt : List ParameterValue -> Arc2d -> List Point2d
pointsAt parameterValues arc =
    List.map (pointOn arc) parameterValues


{-| -}
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


{-| -}
firstDerivativesAt : List ParameterValue -> Arc2d -> List Vector2d
firstDerivativesAt parameterValues arc =
    List.map (firstDerivative arc) parameterValues


{-| -}
type Nondegenerate
    = Nondegenerate Arc2d


{-| -}
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


{-| -}
fromNondegenerate : Nondegenerate -> Arc2d
fromNondegenerate (Nondegenerate arc) =
    arc


{-| -}
tangentDirection : Nondegenerate -> ParameterValue -> Direction2d
tangentDirection (Nondegenerate (Types.Arc2d arc)) parameterValue =
    let
        t =
            ParameterValue.value parameterValue
    in
    arc.xDirection |> Direction2d.rotateBy (t * arc.sweptAngle)


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


{-| -}
toPolyline : { maxError : Float } -> Arc2d -> Polyline2d
toPolyline { maxError } arc =
    let
        numSegments =
            numApproximationSegments maxError arc

        points =
            arc |> pointsAt (ParameterValue.steps numSegments)
    in
    Polyline2d.fromVertices points


{-| -}
reverse : Arc2d -> Arc2d
reverse ((Types.Arc2d arc) as arc_) =
    Types.Arc2d
        { startPoint = endPoint arc_
        , sweptAngle = -arc.sweptAngle
        , signedLength = -arc.signedLength
        , xDirection = arc.xDirection |> Direction2d.rotateBy arc.sweptAngle
        }


{-| -}
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


{-| -}
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


{-| -}
translateBy : Vector2d -> Arc2d -> Arc2d
translateBy displacement (Types.Arc2d arc) =
    Types.Arc2d
        { startPoint = Point2d.translateBy displacement arc.startPoint
        , sweptAngle = arc.sweptAngle
        , signedLength = arc.signedLength
        , xDirection = arc.xDirection
        }


{-| -}
translateIn : Direction2d -> Float -> Arc2d -> Arc2d
translateIn direction distance arc =
    translateBy (Vector2d.withLength distance direction) arc


{-| -}
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


{-| -}
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


{-| -}
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
