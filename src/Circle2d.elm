module Circle2d
    exposing
        ( Circle2d
        , area
        , boundingBox
        , centerPoint
        , circumference
        , contains
        , diameter
        , mirrorAcross
        , placeIn
        , radius
        , relativeTo
        , rotateAround
        , scaleAbout
        , sweptAround
        , throughPoints
        , toArc
        , translateBy
        , translateIn
        , unit
        , withRadius
        )

{-| <img src="https://ianmackenzie.github.io/elm-geometry/1.0.0/Circle2d/icon.svg" alt="Circle2d" width="160">

A `Circle2d` is defined by its center point and radius. This module includes
functionality for

  - Constructing circles through points or with a given center/radius
  - Scaling, rotating and translating circles
  - Extracting properties of circles like area, center point and radius

@docs Circle2d


# Constants

@docs unit


# Constructors

@docs withRadius, throughPoints, sweptAround


# Properties

@docs centerPoint, radius, diameter, area, circumference, boundingBox


# Conversion

@docs toArc


# Queries

@docs contains


# Transformations

@docs scaleAbout, rotateAround, translateBy, translateIn, mirrorAcross


# Coordinate conversions

@docs relativeTo, placeIn

-}

import Axis2d exposing (Axis2d)
import BoundingBox2d exposing (BoundingBox2d)
import Direction2d exposing (Direction2d)
import Frame2d exposing (Frame2d)
import Geometry.Types as Types exposing (Arc2d)
import Point2d exposing (Point2d)
import Vector2d exposing (Vector2d)


{-| -}
type alias Circle2d =
    Types.Circle2d


{-| -}
withRadius : Float -> Point2d -> Circle2d
withRadius radius_ centerPoint_ =
    Types.Circle2d { radius = abs radius_, centerPoint = centerPoint_ }


{-| -}
unit : Circle2d
unit =
    withRadius 1 Point2d.origin


{-| -}
throughPoints : Point2d -> Point2d -> Point2d -> Maybe Circle2d
throughPoints p1 p2 p3 =
    Point2d.circumcenter p1 p2 p3
        |> Maybe.map
            (\p0 ->
                let
                    r1 =
                        Point2d.distanceFrom p0 p1

                    r2 =
                        Point2d.distanceFrom p0 p2

                    r3 =
                        Point2d.distanceFrom p0 p3

                    r =
                        (r1 + r2 + r3) / 3
                in
                withRadius r p0
            )


{-| -}
sweptAround : Point2d -> Point2d -> Circle2d
sweptAround centerPoint_ point =
    withRadius (Point2d.distanceFrom centerPoint_ point) centerPoint_


{-| -}
centerPoint : Circle2d -> Point2d
centerPoint (Types.Circle2d properties) =
    properties.centerPoint


{-| -}
radius : Circle2d -> Float
radius (Types.Circle2d properties) =
    properties.radius


{-| -}
diameter : Circle2d -> Float
diameter circle =
    2 * radius circle


{-| -}
area : Circle2d -> Float
area circle =
    let
        r =
            radius circle
    in
    pi * r * r


{-| -}
circumference : Circle2d -> Float
circumference circle =
    2 * pi * radius circle


{-| -}
toArc : Circle2d -> Arc2d
toArc (Types.Circle2d circle) =
    let
        ( x0, y0 ) =
            Point2d.coordinates circle.centerPoint
    in
    Types.Arc2d
        { startPoint = Point2d.fromCoordinates ( x0 + circle.radius, y0 )
        , xDirection = Direction2d.y
        , sweptAngle = 2 * pi
        , signedLength = 2 * pi * circle.radius
        }


{-| -}
contains : Point2d -> Circle2d -> Bool
contains point circle =
    let
        r =
            radius circle
    in
    Point2d.squaredDistanceFrom (centerPoint circle) point <= r * r


{-| -}
scaleAbout : Point2d -> Float -> Circle2d -> Circle2d
scaleAbout point scale (Types.Circle2d circle) =
    withRadius (abs scale * circle.radius)
        (Point2d.scaleAbout point scale circle.centerPoint)


{-| -}
rotateAround : Point2d -> Float -> Circle2d -> Circle2d
rotateAround point angle =
    let
        rotatePoint =
            Point2d.rotateAround point angle
    in
    \(Types.Circle2d circle) ->
        withRadius circle.radius (rotatePoint circle.centerPoint)


{-| -}
translateBy : Vector2d -> Circle2d -> Circle2d
translateBy displacement (Types.Circle2d circle) =
    withRadius circle.radius
        (Point2d.translateBy displacement circle.centerPoint)


{-| -}
translateIn : Direction2d -> Float -> Circle2d -> Circle2d
translateIn direction distance circle =
    translateBy (Vector2d.withLength distance direction) circle


{-| -}
mirrorAcross : Axis2d -> Circle2d -> Circle2d
mirrorAcross axis (Types.Circle2d circle) =
    withRadius circle.radius (Point2d.mirrorAcross axis circle.centerPoint)


{-| -}
relativeTo : Frame2d -> Circle2d -> Circle2d
relativeTo frame (Types.Circle2d circle) =
    withRadius circle.radius (Point2d.relativeTo frame circle.centerPoint)


{-| -}
placeIn : Frame2d -> Circle2d -> Circle2d
placeIn frame (Types.Circle2d circle) =
    withRadius circle.radius (Point2d.placeIn frame circle.centerPoint)


{-| -}
boundingBox : Circle2d -> BoundingBox2d
boundingBox circle =
    let
        ( x, y ) =
            Point2d.coordinates (centerPoint circle)

        r =
            radius circle
    in
    BoundingBox2d.fromExtrema
        { minX = x - r
        , maxX = x + r
        , minY = y - r
        , maxY = y + r
        }
