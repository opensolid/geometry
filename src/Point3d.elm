--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- This Source Code Form is subject to the terms of the Mozilla Public        --
-- License, v. 2.0. If a copy of the MPL was not distributed with this file,  --
-- you can obtain one at http://mozilla.org/MPL/2.0/.                         --
--                                                                            --
-- Copyright 2016 by Ian Mackenzie                                            --
-- ian.e.mackenzie@gmail.com                                                  --
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------


module Point3d
    exposing
        ( Point3d
        , along
        , circumcenter
        , coordinates
        , distanceFrom
        , distanceFromAxis
        , equalWithin
        , fromCoordinates
        , fromCoordinatesIn
        , interpolateFrom
        , midpoint
        , mirrorAcross
        , on
        , origin
        , placeIn
        , projectInto
        , projectOnto
        , projectOntoAxis
        , relativeTo
        , rotateAround
        , scaleAbout
        , signedDistanceAlong
        , signedDistanceFrom
        , squaredDistanceFrom
        , squaredDistanceFromAxis
        , translateBy
        , translateIn
        , xCoordinate
        , yCoordinate
        , zCoordinate
        )

{-| <img src="https://ianmackenzie.github.io/elm-geometry/1.0.0/Point3d/icon.svg" alt="Point3d" width="160">

A `Point3d` represents a position in 3D space and is defined by its X, Y and Z
coordinates. This module contains a variety of point-related functionality, such
as

  - Measuring distance between points, or the distance of a point from an axis
    or a plane
  - Scaling, rotating, translating, mirroring and projecting points
  - Converting points between different coordinate systems

Points are distinct from vectors but interact with them in well-defined ways;
you can translate a point by a vector to result in a new point, or you can
compute the vector from one point to another, but you cannot 'add' two points
like you can add two vectors.

@docs Point3d


# Constants

@docs origin


# Constructors

@docs fromCoordinates, fromCoordinatesIn, midpoint, interpolateFrom, along, on, circumcenter


# Properties

@docs coordinates, xCoordinate, yCoordinate, zCoordinate


# Comparison

@docs equalWithin


# Measurement

@docs distanceFrom, squaredDistanceFrom, signedDistanceAlong, distanceFromAxis, squaredDistanceFromAxis, signedDistanceFrom


# Transformations

@docs scaleAbout, rotateAround, translateBy, translateIn, mirrorAcross, projectOnto, projectOntoAxis


# Coordinate conversions

Functions for transforming points between local and global coordinates in
different coordinate frames.

@docs relativeTo, placeIn, projectInto

-}

import Bootstrap.Frame3d as Frame3d
import Bootstrap.Plane3d as Plane3d
import Bootstrap.SketchPlane3d as SketchPlane3d
import Direction3d exposing (Direction3d)
import Float.Extra as Float
import Geometry.Types as Types exposing (Axis3d, Frame3d, Plane3d, SketchPlane3d)
import Point2d exposing (Point2d)
import Vector3d exposing (Vector3d)


addTo : Point3d -> Vector3d -> Point3d
addTo point vector =
    translateBy vector point


{-| -}
type alias Point3d =
    Types.Point3d


{-| The point (0, 0, 0).

    Point3d.origin
    --> Point3d.fromCoordinates ( 0, 0, 0 )

-}
origin : Point3d
origin =
    fromCoordinates ( 0, 0, 0 )


{-| Construct a point from its X, Y and Z coordinates.

    point =
        Point3d.fromCoordinates ( 2, 1, 3 )

-}
fromCoordinates : ( Float, Float, Float ) -> Point3d
fromCoordinates =
    Types.Point3d


{-| Construct a point halfway between two other points.

    p1 =
        Point3d.fromCoordinates ( 1, 1, 1 )

    p2 =
        Point3d.fromCoordinates ( 3, 7, 9 )

    Point3d.midpoint p1 p2
    --> Point3d.fromCoordinates ( 2, 4, 5 )

-}
midpoint : Point3d -> Point3d -> Point3d
midpoint firstPoint secondPoint =
    interpolateFrom firstPoint secondPoint 0.5


{-| Construct a point by interpolating from the first given point to the second,
based on a parameter that ranges from zero to one.

    startPoint =
        Point3d.fromCoordinates ( 1, 2, 4 )

    endPoint =
        Point3d.fromCoordinates ( 1, 2, 8 )

    Point3d.interpolateFrom startPoint endPoint 0.25
    --> Point3d.fromCoordinates ( 1, 2, 5 )

Partial application may be useful:

    interpolatedPoint : Float -> Point3d
    interpolatedPoint =
        Point3d.interpolateFrom startPoint endPoint

    List.map interpolatedPoint [ 0, 0.5, 1 ]
    --> [ Point3d.fromCoordinates ( 1, 2, 4 )
    --> , Point3d.fromCoordinates ( 1, 2, 6 )
    --> , Point3d.fromCoordinates ( 1, 2, 8 )
    --> ]

You can pass values less than zero or greater than one to extrapolate:

    interpolatedPoint -0.5
    --> Point3d.fromCoordinates ( 1, 2, 2 )

    interpolatedPoint 1.25
    --> Point3d.fromCoordinates ( 1, 2, 9 )

-}
interpolateFrom : Point3d -> Point3d -> Float -> Point3d
interpolateFrom p1 p2 t =
    let
        ( x1, y1, z1 ) =
            coordinates p1

        ( x2, y2, z2 ) =
            coordinates p2
    in
    fromCoordinates
        ( Float.interpolateFrom x1 x2 t
        , Float.interpolateFrom y1 y2 t
        , Float.interpolateFrom z1 z2 t
        )


{-| Construct a point along an axis at a particular distance from the axis'
origin point.

    Point3d.along Axis3d.z 2
    --> Point3d.fromCoordinates ( 0, 0, 2 )

Positive and negative distances are interpreted relative to the direction of the
axis:

    horizontalAxis =
        Axis3d.withDirection Direction3d.negativeX
            (Point3d.fromCoordinates ( 1, 1, 1 ))

    Point3d.along horizontalAxis 3
    --> Point3d.fromCoordinates ( -2, 1, 1 )

    Point3d.along horizontalAxis -3
    --> Point3d.fromCoordinates ( 4, 1, 1 )

-}
along : Axis3d -> Float -> Point3d
along (Types.Axis3d axis) distance =
    axis.originPoint
        |> translateBy (Vector3d.withLength distance axis.direction)


{-| Construct a 3D point lying _on_ a sketch plane by providing a 2D point
specified in XY coordinates _within_ the sketch plane.

    Point3d.on SketchPlane3d.xy <|
        Point2d.fromCoordinates ( 2, 1 )
    --> Point3d.fromCoordinates ( 2, 1, 0 )

    Point3d.on SketchPlane3d.xz <|
        Point2d.fromCoordinates ( 2, 1 )
    --> Point3d.fromCoordinates ( 2, 0, 1 )

The sketch plane can have any position and orientation:

    tiltedSketchPlane =
        SketchPlane3d.xy
            |> SketchPlane3d.rotateAround Axis3d.x
                (degrees 45)
            |> SketchPlane3d.moveTo
                (Point3d.fromCoordinates ( 10, 10, 10 ))

    Point3d.on tiltedSketchPlane <|
        Point2d.fromCoordinates ( 2, 1 )
    --> Point3d.fromCoordinates ( 12, 10.7071, 10.7071 )

-}
on : SketchPlane3d -> Point2d -> Point3d
on sketchPlane point2d =
    let
        ( x0, y0, z0 ) =
            coordinates (SketchPlane3d.originPoint sketchPlane)

        ( ux, uy, uz ) =
            Direction3d.components (SketchPlane3d.xDirection sketchPlane)

        ( vx, vy, vz ) =
            Direction3d.components (SketchPlane3d.yDirection sketchPlane)

        ( x, y ) =
            Point2d.coordinates point2d
    in
    fromCoordinates
        ( x0 + x * ux + y * vx
        , y0 + x * uy + y * vy
        , z0 + x * uz + y * vz
        )


{-| Construct a point given its local coordinates within a particular frame.

    frame =
        Frame3d.atPoint
            (Point3d.fromCoordinates ( 1, 1, 1 ))

    Point3d.fromCoordinatesIn frame ( 1, 2, 3 )
    --> Point3d.fromCoordinates ( 2, 3, 4 )

This is shorthand for using `Point3d.placeIn`;

    Point3d.fromCoordinatesIn frame localCoordinates

is equivalent to

    Point3d.fromCoordinates localCoordinates
        |> Point3d.placeIn frame

-}
fromCoordinatesIn : Frame3d -> ( Float, Float, Float ) -> Point3d
fromCoordinatesIn frame localCoordinates =
    placeIn frame (fromCoordinates localCoordinates)


{-| Attempt to find the circumcenter of three points; this is the center of the
circle that passes through all three points. If the three given points are
collinear, returns `Nothing`.

    Point3d.circumcenter
        (Point3d.fromCoordinates ( 1, 0, 0 ))
        (Point3d.fromCoordinates ( 0, 1, 0 ))
        (Point3d.fromCoordinates ( 0, 0, 1 ))
    --> Just (Point3d.fromCoordinates (0.33, 0.33, 0.33))

    Point3d.circumcenter
        Point3d.origin
        (Point3d.fromCoordinates ( 1, 0, 0 ))
        (Point3d.fromCoordinates ( 2, 0, 0 ))
    --> Nothing

-}
circumcenter : Point3d -> Point3d -> Point3d -> Maybe Point3d
circumcenter p1 p2 p3 =
    let
        a2 =
            squaredDistanceFrom p1 p2

        b2 =
            squaredDistanceFrom p2 p3

        c2 =
            squaredDistanceFrom p3 p1

        t1 =
            a2 * (b2 + c2 - a2)

        t2 =
            b2 * (c2 + a2 - b2)

        t3 =
            c2 * (a2 + b2 - c2)

        sum =
            t1 + t2 + t3
    in
    if sum == 0 then
        Nothing
    else
        let
            w1 =
                t1 / sum

            w2 =
                t2 / sum

            w3 =
                t3 / sum

            ( x1, y1, z1 ) =
                coordinates p1

            ( x2, y2, z2 ) =
                coordinates p2

            ( x3, y3, z3 ) =
                coordinates p3
        in
        Just <|
            fromCoordinates
                ( w1 * x3 + w2 * x1 + w3 * x2
                , w1 * y3 + w2 * y1 + w3 * y2
                , w1 * z3 + w2 * z1 + w3 * z2
                )


{-| Get the coordinates of a point as a tuple.

    ( x, y, z ) =
        Point3d.coordinates point

-}
coordinates : Point3d -> ( Float, Float, Float )
coordinates (Types.Point3d coordinates_) =
    coordinates_


{-| Get the X coordinate of a point.

    Point3d.fromCoordinates ( 2, 1, 3 )
        |> Point3d.xCoordinate
    --> 2

-}
xCoordinate : Point3d -> Float
xCoordinate (Types.Point3d ( x, _, _ )) =
    x


{-| Get the Y coordinate of a point.

    Point3d.fromCoordinates ( 2, 1, 3 )
        |> Point3d.yCoordinate
    --> 1

-}
yCoordinate : Point3d -> Float
yCoordinate (Types.Point3d ( _, y, _ )) =
    y


{-| Get the Z coordinate of a point.

    Point3d.fromCoordinates ( 2, 1, 3 )
        |> Point3d.zCoordinate
    --> 3

-}
zCoordinate : Point3d -> Float
zCoordinate (Types.Point3d ( _, _, z )) =
    z


{-| Compare two points within a tolerance. Returns true if the distance
between the two given points is less than the given tolerance.

    firstPoint =
        Point3d.fromCoordinates ( 2, 1, 3 )

    secondPoint =
        Point3d.fromCoordinates ( 2.0002, 0.9999, 3.0001 )

    Point3d.equalWithin 1e-3 firstPoint secondPoint
    --> True

    Point3d.equalWithin 1e-6 firstPoint secondPoint
    --> False

-}
equalWithin : Float -> Point3d -> Point3d -> Bool
equalWithin tolerance firstPoint secondPoint =
    squaredDistanceFrom firstPoint secondPoint <= tolerance * tolerance


{-| Find the distance from the first point to the second.

    p1 =
        Point3d.fromCoordinates ( 1, 1, 2 )

    p2 =
        Point3d.fromCoordinates ( 2, 3, 4 )

    Point3d.distanceFrom p1 p2
    --> 3

Partial application can be useful:

    points =
        [ Point3d.fromCoordinates ( 3, 4, 5 )
        , Point3d.fromCoordinates ( 10, 10, 10 )
        , Point3d.fromCoordinates ( -1, 2, -3 )
        ]

    points
        |> List.sortBy
            (Point3d.distanceFrom Point3d.origin)
    --> [ Point3d.fromCoordinates ( -1, 2, -3 )
    --> , Point3d.fromCoordinates ( 3, 4, 5 )
    --> , Point3d.fromCoordinates ( 10, 10, 10 )
    --> ]

-}
distanceFrom : Point3d -> Point3d -> Float
distanceFrom firstPoint secondPoint =
    sqrt (squaredDistanceFrom firstPoint secondPoint)


{-| Find the square of the distance from one point to another.
`squaredDistanceFrom` is slightly faster than `distanceFrom`, so for example

    Point3d.squaredDistanceFrom p1 p2
        > tolerance * tolerance

is equivalent to but slightly more efficient than

    Point3d.distanceFrom p1 p2 > tolerance

since the latter requires a square root under the hood. In many cases, however,
the speed difference will be negligible and using `distanceFrom` is much more
readable!

-}
squaredDistanceFrom : Point3d -> Point3d -> Float
squaredDistanceFrom firstPoint secondPoint =
    Vector3d.squaredLength (Vector3d.from firstPoint secondPoint)


{-| Determine how far along an axis a particular point lies. Conceptually, the
point is projected perpendicularly onto the axis, and then the distance of this
projected point from the axis' origin point is measured. The result will be
positive if the projected point is ahead the axis' origin point and negative if
it is behind, with 'ahead' and 'behind' defined by the direction of the axis.

    axis =
        Axis3d.withDirection Direction3d.x
            (Point3d.fromCoordinates ( 1, 0, 0 ))

    point =
        Point3d.fromCoordinates ( 3, 3, 3 )

    Point3d.signedDistanceAlong axis point
    --> 2

    Point3d.signedDistanceAlong axis Point3d.origin
    --> -1

-}
signedDistanceAlong : Axis3d -> Point3d -> Float
signedDistanceAlong (Types.Axis3d axis) point =
    Vector3d.from axis.originPoint point |> Vector3d.componentIn axis.direction


{-| Find the perpendicular (nearest) distance of a point from an axis.

    point =
        Point3d.fromCoordinates ( -3, 4, 0 )

    Point3d.distanceFromAxis Axis3d.x point
    --> 4

    Point3d.distanceFromAxis Axis3d.y point
    --> 3

    Point3d.distanceFromAxis Axis3d.z point
    --> 5

Note that unlike in 2D, the result is always positive (unsigned) since there is
no such thing as the left or right side of an axis in 3D.

-}
distanceFromAxis : Axis3d -> Point3d -> Float
distanceFromAxis axis point =
    sqrt (squaredDistanceFromAxis axis point)


{-| Find the square of the perpendicular distance of a point from an axis. As
with `distanceFrom`/`squaredDistanceFrom` this is slightly more efficient than
`distanceFromAxis` since it avoids a square root.
-}
squaredDistanceFromAxis : Axis3d -> Point3d -> Float
squaredDistanceFromAxis (Types.Axis3d axis) point =
    Vector3d.from axis.originPoint point
        |> Vector3d.crossProduct (Direction3d.toVector axis.direction)
        |> Vector3d.squaredLength


{-| Find the perpendicular distance of a point from a plane. The result will be
positive if the point is 'above' the plane and negative if it is 'below', with
'up' defined by the normal direction of the plane.

    plane =
        Plane3d.withNormalDirection Direction3d.y
            (Point3d.fromCoordinates ( 1, 2, 3 ))

    point =
        Point3d.fromCoordinates ( 3, 3, 3 )

    Point3d.signedDistanceFrom plane point
    --> 1

    Point3d.signedDistanceFrom plane Point3d.origin
    --> -2

This means that flipping a plane (reversing its normal direction) will also flip
the sign of the result of this function:

    flippedPlane =
        Plane3d.reverseNormal plane

    Point3d.signedDistanceFrom flippedPlane point
    --> -1

    Point3d.signedDistanceFrom flippedPlane Point3d.origin
    --> 2

-}
signedDistanceFrom : Plane3d -> Point3d -> Float
signedDistanceFrom plane point =
    let
        ( x, y, z ) =
            coordinates point

        ( x0, y0, z0 ) =
            coordinates (Plane3d.originPoint plane)

        ( nx, ny, nz ) =
            Direction3d.components (Plane3d.normalDirection plane)
    in
    (x - x0) * nx + (y - y0) * ny + (z - z0) * nz


{-| Perform a uniform scaling about the given center point. The center point is
given first and the point to transform is given last. Points will contract or
expand about the center point by the given scale. Scaling by a factor of 1 is a
no-op, and scaling by a factor of 0 collapses all points to the center point.

    centerPoint =
        Point3d.fromCoordinates ( 1, 1, 1 )

    point =
        Point3d.fromCoordinates ( 1, 2, 3 )

    Point3d.scaleAbout centerPoint 3 point
    --> Point3d.fromCoordinates ( 1, 4, 7 )

    Point3d.scaleAbout centerPoint 0.5 point
    --> Point3d.fromCoordinates ( 1, 1.5, 2 )

Avoid scaling by a negative scaling factor - while this may sometimes do what
you want it is confusing and error prone. Try a combination of mirror and/or
rotation operations instead.

-}
scaleAbout : Point3d -> Float -> Point3d -> Point3d
scaleAbout centerPoint scale point =
    Vector3d.from centerPoint point
        |> Vector3d.scaleBy scale
        |> addTo centerPoint


{-| Rotate a point around an axis by a given angle (in radians).

    axis =
        Axis3d.x

    angle =
        degrees 45

    point =
        Point3d.fromCoordinates ( 3, 1, 0 )

    Point3d.rotateAround axis angle point
    --> Point3d.fromCoordinates ( 3, 0.7071, 0.7071 )

Rotation direction is given by the right-hand rule, counterclockwise around the
direction of the axis.

-}
rotateAround : Axis3d -> Float -> Point3d -> Point3d
rotateAround ((Types.Axis3d axis) as axis_) angle point =
    Vector3d.from axis.originPoint point
        |> Vector3d.rotateAround axis_ angle
        |> addTo axis.originPoint


{-| Translate a point by a given displacement.

    point =
        Point3d.fromCoordinates ( 3, 4, 5 )

    displacement =
        Vector3d.fromComponents ( 1, 2, 3 )

    Point3d.translateBy displacement point
    --> Point3d.fromCoordinates ( 4, 6, 8 )

-}
translateBy : Vector3d -> Point3d -> Point3d
translateBy vector point =
    let
        ( vx, vy, vz ) =
            Vector3d.components vector

        ( px, py, pz ) =
            coordinates point
    in
    fromCoordinates ( px + vx, py + vy, pz + vz )


{-| Translate a point in a given direction by a given distance.

    point =
        Point3d.fromCoordinates ( 3, 4, 5 )

    point |> Point3d.translateIn Direction3d.x 2
    --> Point3d.fromCoordinates ( 5, 4, 5 )

    point |> Point3d.translateIn Direction3d.y 2
    --> Point3d.fromCoordinates ( 3, 6, 5 )

The distance can be negative:

    Point3d.translateIn Direction3d.x -2
    --> Point3d.fromCoordinates ( 1, 4, 5 )

-}
translateIn : Direction3d -> Float -> Point3d -> Point3d
translateIn direction distance point =
    let
        ( dx, dy, dz ) =
            Direction3d.components direction

        ( px, py, pz ) =
            coordinates point
    in
    fromCoordinates
        ( px + distance * dx
        , py + distance * dy
        , pz + distance * dz
        )


{-| Mirror a point across a plane. The result will be the same distance from the
plane but on the opposite side.

    point =
        Point3d.fromCoordinates ( 1, 2, 3 )

    -- Plane3d.xy is the plane Z=0
    Point3d.mirrorAcross Plane3d.xy point
    --> Point3d.fromCoordinates ( 1, 2, -3 )

    -- Plane3d.yz is the plane X=0
    Point3d.mirrorAcross Plane3d.yz point
    --> Point3d.fromCoordinates ( -1, 2, 3 )

The plane does not have to pass through the origin:

    -- offsetPlane is the plane Z=1
    offsetPlane =
        Plane3d.offsetBy 1 Plane3d.xy

    -- The origin point is 1 unit below the offset
    -- plane, so its mirrored copy is one unit above
    Point3d.mirrorAcross offsetPlane Point3d.origin
    --> Point3d.fromCoordinates ( 0, 0, 2 )

-}
mirrorAcross : Plane3d -> Point3d -> Point3d
mirrorAcross plane point =
    let
        originPoint =
            Plane3d.originPoint plane
    in
    Vector3d.from originPoint point
        |> Vector3d.mirrorAcross plane
        |> addTo originPoint


{-| Find the [orthographic projection](https://en.wikipedia.org/wiki/Orthographic_projection)
of a point onto a plane:

    point =
        Point3d.fromCoordinates ( 1, 2, 3 )

    Point3d.projectOnto Plane3d.xy point
    --> Point3d.fromCoordinates ( 1, 2, 0 )

    Point3d.projectOnto Plane3d.yz point
    --> Point3d.fromCoordinates ( 0, 2, 3 )

The plane does not have to pass through the origin:

    offsetPlane =
        Plane3d.offsetBy 1 Plane3d.xy

    Point3d.projectOnto offsetPlane point
    --> Point3d.fromCoordinates ( 1, 2, 1 )

-}
projectOnto : Plane3d -> Point3d -> Point3d
projectOnto plane point =
    let
        displacement =
            Vector3d.withLength -(signedDistanceFrom plane point)
                (Plane3d.normalDirection plane)
    in
    translateBy displacement point


{-| Project a point perpendicularly onto an axis.

    point =
        Point3d.fromCoordinates ( 1, 2, 3 )

    Point3d.projectOntoAxis Axis3d.x
    --> Point3d.fromCoordinates ( 1, 0, 0 )

    verticalAxis =
        Axis3d.withDirection Direction3d.z
            (Point3d.fromCoordinates ( 0, 1, 2 ))

    Point3d.projectOntoAxis verticalAxis
    --> Point3d.fromCoordinates ( 0, 1, 3 )

-}
projectOntoAxis : Axis3d -> Point3d -> Point3d
projectOntoAxis axis point =
    along axis (signedDistanceAlong axis point)


{-| Take a point defined in global coordinates, and return it expressed in local
coordinates relative to a given reference frame.

    localFrame =
        Frame3d.atPoint
            (Point3d.fromCoordinates ( 1, 2, 3 ))

    Point3d.relativeTo localFrame
        (Point3d.fromCoordinates ( 4, 5, 6 ))
    --> Point3d.fromCoordinates ( 3, 3, 3 )

    Point3d.relativeTo localFrame
        (Point3d.fromCoordinates ( 1, 1, 1 ))
    --> Point3d.fromCoordinates ( 0, -1, -2 )

-}
relativeTo : Frame3d -> Point3d -> Point3d
relativeTo frame point =
    Vector3d.from (Frame3d.originPoint frame) point
        |> Vector3d.relativeTo frame
        |> Vector3d.components
        |> fromCoordinates


{-| Take a point defined in local coordinates relative to a given reference
frame, and return that point expressed in global coordinates.

    localFrame =
        Frame3d.atPoint
            (Point3d.fromCoordinates ( 1, 2, 3 ))

    Point3d.placeIn localFrame
        (Point3d.fromCoordinates ( 3, 3, 3 ))
    --> Point3d.fromCoordinates ( 4, 5, 6 )

    Point3d.placeIn localFrame
        (Point3d.fromCoordinates ( 0, -1, -2 ))
    --> Point3d.fromCoordinates ( 1, 1, 1 )

-}
placeIn : Frame3d -> Point3d -> Point3d
placeIn frame point =
    Vector3d.fromComponents (coordinates point)
        |> Vector3d.placeIn frame
        |> addTo (Frame3d.originPoint frame)


{-| Project a point into a given sketch plane. Conceptually, this finds the
[orthographic projection](https://en.wikipedia.org/wiki/Orthographic_projection)
of the point onto the plane and then expresses the projected point in 2D sketch
coordinates.

    point =
        Point3d.fromCoordinates ( 2, 1, 3 )

    Point3d.projectInto SketchPlane3d.xy point
    --> Point2d.fromCoordinates ( 2, 1 )

    Point3d.projectInto SketchPlane3d.yz point
    --> Point2d.fromCoordinates ( 1, 3 )

    Point3d.projectInto SketchPlane3d.zx point
    --> Point2d.fromCoordinates ( 3, 2 )

-}
projectInto : SketchPlane3d -> Point3d -> Point2d
projectInto sketchPlane point =
    let
        ( x, y, z ) =
            coordinates point

        ( x0, y0, z0 ) =
            coordinates (SketchPlane3d.originPoint sketchPlane)

        ( ux, uy, uz ) =
            Direction3d.components (SketchPlane3d.xDirection sketchPlane)

        ( vx, vy, vz ) =
            Direction3d.components (SketchPlane3d.yDirection sketchPlane)

        dx =
            x - x0

        dy =
            y - y0

        dz =
            z - z0
    in
    Point2d.fromCoordinates
        ( dx * ux + dy * uy + dz * uz
        , dx * vx + dy * vy + dz * vz
        )
