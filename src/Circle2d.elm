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


{-| Construct a circle from its radius and center point:

    exampleCircle =
        Circle2d.withRadius 3
            (Point2d.fromCoordinates ( 1, 2 ))

If you pass a negative radius, the absolute value will be used.

-}
withRadius : Float -> Point2d -> Circle2d
withRadius radius_ centerPoint_ =
    Types.Circle2d { radius = abs radius_, centerPoint = centerPoint_ }


{-| The unit circle, centered on the origin with a radius of 1.

    Circle2d.unit
    --> Circle2d.withRadius 1 Point2d.origin

-}
unit : Circle2d
unit =
    withRadius 1 Point2d.origin


{-| Attempt to construct a circle that passes through the three given points. If
the three given points are collinear, returns `Nothing`.

    Circle2d.throughPoints
        Point2d.origin
        (Point2d.fromCoordinates ( 1, 0 ))
        (Point2d.fromCoordinates ( 0, 1 ))
    --> Just
    -->     (Circle2d.withRadius 0.7071
    -->         (Point2d.fromCoordinates ( 0.5, 0.5 ))
    -->     )

    Circle2d.throughPoints
        Point2d.origin
        (Point2d.fromCoordinates ( 2, 1 ))
        (Point2d.fromCoordinates ( 4, 0 ))
    --> Just
    -->     (Circle2d.withRadius 2.5
    -->         (Point2d.fromCoordinates ( 2, -1.5 ))
    -->     )

    Circle2d.throughPoints
        Point2d.origin
        (Point2d.fromCoordinates ( 2, 0 ))
        (Point2d.fromCoordinates ( 4, 0 ))
    --> Nothing

    Circle2d.throughPoints
        Point2d.origin
        Point2d.origin
        (Point2d.fromCoordinates ( 1, 0 ))
    --> Nothing

-}
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


{-| Construct a circle by rotating a point on the circle around a given center
point. The center point is given first and the point on the circle is given
second.

    Circle2d.sweptAround Point2d.origin
        (Point2d.fromCoordinates ( 2, 0 ))
    --> Circle2d.withRadius 2 Point2d.origin

The above example could be rewritten as

    Point2d.fromCoordinates ( 2, 0 )
        |> Circle2d.sweptAround Point2d.origin

and if you wanted to create many concentric circles all centered on the origin
but passing through several other different points, you could use something like

    concentricCircles =
        points
            |> List.map
                (Circle2d.sweptAround Point2d.origin)

-}
sweptAround : Point2d -> Point2d -> Circle2d
sweptAround centerPoint_ point =
    withRadius (Point2d.distanceFrom centerPoint_ point) centerPoint_


{-| Get the center point of a circle.

    Circle2d.centerPoint exampleCircle
    --> Point2d.fromCoordinates ( 1, 2 )

-}
centerPoint : Circle2d -> Point2d
centerPoint (Types.Circle2d properties) =
    properties.centerPoint


{-| Get the radius of a circle.

    Circle2d.radius exampleCircle
    --> 3

-}
radius : Circle2d -> Float
radius (Types.Circle2d properties) =
    properties.radius


{-| Get the diameter of a circle.

    Circle2d.diameter exampleCircle
    --> 6

-}
diameter : Circle2d -> Float
diameter circle =
    2 * radius circle


{-| Get the area of a circle.

    Circle2d.area exampleCircle
    --> 28.2743

-}
area : Circle2d -> Float
area circle =
    let
        r =
            radius circle
    in
    pi * r * r


{-| Get the circumference of a circle.

    Circle2d.circumference exampleCircle
    --> 18.8496

-}
circumference : Circle2d -> Float
circumference circle =
    2 * pi * radius circle


{-| Convert a circle to a 360 degree arc.

    Circle2d.toArc exampleCircle
    --> Point2d.fromCoordinates ( 4, 2 )
    -->     |> Arc2d.sweptAround
    -->         (Point2d.fromCoordinates ( 1, 2 ))
    -->         (degrees 360)

-}
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


{-| Check if a circle contains a given point.

    Circle2d.contains Point2d.origin exampleCircle
    --> True

    exampleCircle
        |> Circle2d.contains
            (Point2d.fromCoordinates ( 10, 10 ))
    --> False

-}
contains : Point2d -> Circle2d -> Bool
contains point circle =
    let
        r =
            radius circle
    in
    Point2d.squaredDistanceFrom (centerPoint circle) point <= r * r


{-| Scale a circle about a given point by a given scale.

    Circle2d.scaleAbout Point2d.origin 2 exampleCircle
    --> Circle2d.withRadius 6
    -->     (Point2d.fromCoordinates ( 2, 4 ))

    exampleCircle
        |> Circle2d.scaleAbout
            (Point2d.fromCoordinates ( 1, 2 ))
            0.5
    --> Circle2d.withRadius 1.5
    -->     (Point2d.fromCoordinates ( 1, 2 ))

-}
scaleAbout : Point2d -> Float -> Circle2d -> Circle2d
scaleAbout point scale (Types.Circle2d circle) =
    withRadius (abs scale * circle.radius)
        (Point2d.scaleAbout point scale circle.centerPoint)


{-| Rotate a circle around a given point by a given angle (in radians).

    exampleCircle
        |> Circle2d.rotateAround Point2d.origin
            (degrees 90)
    --> Circle2d.withRadius 3
    -->     (Point2d.fromCoordinates ( -2, 1 ))

-}
rotateAround : Point2d -> Float -> Circle2d -> Circle2d
rotateAround point angle =
    let
        rotatePoint =
            Point2d.rotateAround point angle
    in
    \(Types.Circle2d circle) ->
        withRadius circle.radius (rotatePoint circle.centerPoint)


{-| Translate a circle by a given displacement.

    exampleCircle
        |> Circle2d.translateBy
            (Vector2d.fromComponents ( 2, 2 ))
    --> Circle2d.withRadius 3
    -->     (Point2d.fromCoordinates ( 3, 4 ))

-}
translateBy : Vector2d -> Circle2d -> Circle2d
translateBy displacement (Types.Circle2d circle) =
    withRadius circle.radius
        (Point2d.translateBy displacement circle.centerPoint)


{-| Translate a circle in a given direction by a given distance;

    Circle2d.translateIn direction distance

is equivalent to

    Circle2d.translateBy
        (Vector2d.withLength distance direction)

-}
translateIn : Direction2d -> Float -> Circle2d -> Circle2d
translateIn direction distance circle =
    translateBy (Vector2d.withLength distance direction) circle


{-| Mirror a circle across a given axis.

    Circle2d.mirrorAcross Axis2d.x exampleCircle
    --> Circle2d.withRadius 3
    -->     (Point2d.fromCoordinates ( 1, -2 ))

-}
mirrorAcross : Axis2d -> Circle2d -> Circle2d
mirrorAcross axis (Types.Circle2d circle) =
    withRadius circle.radius (Point2d.mirrorAcross axis circle.centerPoint)


{-| Take a circle defined in global coordinates, and return it expressed in
local coordinates relative to a given reference frame.

    localFrame =
        Frame2d.atPoint (Point2d.fromCoordinates ( 2, 3 ))

    Circle2d.relativeTo localFrame exampleCircle
    --> Circle2d.withRadius 3
    -->     (Point2d.fromCoordinates ( -1, -1 ))

-}
relativeTo : Frame2d -> Circle2d -> Circle2d
relativeTo frame (Types.Circle2d circle) =
    withRadius circle.radius (Point2d.relativeTo frame circle.centerPoint)


{-| Take a circle considered to be defined in local coordinates relative to a
given reference frame, and return that circle expressed in global coordinates.

    localFrame =
        Frame2d.atPoint (Point2d.fromCoordinates ( 2, 3 ))

    Circle2d.placeIn localFrame exampleCircle
    --> Circle2d.withRadius 3
    -->     (Point2d.fromCoordinates ( 3, 5 ))

-}
placeIn : Frame2d -> Circle2d -> Circle2d
placeIn frame (Types.Circle2d circle) =
    withRadius circle.radius (Point2d.placeIn frame circle.centerPoint)


{-| Get the minimal bounding box containing a given circle.

    Circle2d.boundingBox exampleCircle
    --> BoundingBox2d.fromExtrema
    -->     { minX = -2
    -->     , maxX = 4
    -->     , minY = -1
    -->     , maxY = 5
    -->     }

-}
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
