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


module LineSegment3d
    exposing
        ( LineSegment3d
        , along
        , boundingBox
        , direction
        , endPoint
        , endpoints
        , from
        , fromEndpoints
        , interpolate
        , length
        , mapEndpoints
        , midpoint
        , mirrorAcross
        , on
        , perpendicularDirection
        , placeIn
        , projectInto
        , projectOnto
        , relativeTo
        , reverse
        , rotateAround
        , scaleAbout
        , squaredLength
        , startPoint
        , translateBy
        , translateIn
        , vector
        )

{-| <img src="https://ianmackenzie.github.io/elm-geometry/1.0.0/LineSegment3d/icon.svg" alt="LineSegment3d" width="160">

A `LineSegment3d` is a line between two points in 3D. This module contains
functionality such as:

  - Interpolating within a line segment or measuring its length
  - Scaling, rotating, translating, mirroring or projecting a line segment
  - Converting a line segment between local and global coordinates in different
    reference frames

@docs LineSegment3d


# Constructors

@docs fromEndpoints, from, along, on


# Properties

@docs startPoint, endPoint, endpoints, midpoint, length, squaredLength, direction, perpendicularDirection, vector, boundingBox


# Interpolation

@docs interpolate


# Transformations

Transforming a line segment is equivalent to transforming its start and end
points and forming a new line segment between the resulting points.

@docs reverse, scaleAbout, rotateAround, translateBy, translateIn, mirrorAcross, projectOnto, mapEndpoints


# Coordinate conversions

Functions for transforming line segments between local and global coordinates in
different coordinate frames.

@docs relativeTo, placeIn, projectInto

-}

import Axis3d exposing (Axis3d)
import BoundingBox3d exposing (BoundingBox3d)
import Direction3d exposing (Direction3d)
import Frame3d exposing (Frame3d)
import Geometry.Types as Types
import LineSegment2d exposing (LineSegment2d)
import Plane3d exposing (Plane3d)
import Point3d exposing (Point3d)
import SketchPlane3d exposing (SketchPlane3d)
import Vector3d exposing (Vector3d)


{-| -}
type alias LineSegment3d =
    Types.LineSegment3d


{-| Construct a line segment from its two endpoints:

    exampleLineSegment =
        LineSegment3d.fromEndpoints
            ( Point3d.fromCoordinates ( 1, 2, 3 )
            , Point3d.fromCoordinates ( 4, 5, 6 )
            )

-}
fromEndpoints : ( Point3d, Point3d ) -> LineSegment3d
fromEndpoints =
    Types.LineSegment3d


{-| Construct a line segment from the first point to the second;

    LineSegment3d.from firstPoint secondPoint

is equivalent to

    LineSegment3d.fromEndpoints ( firstPoint, secondPoint )

-}
from : Point3d -> Point3d -> LineSegment3d
from startPoint_ endPoint_ =
    fromEndpoints ( startPoint_, endPoint_ )


{-| Construct a line segment lying on the given axis, with its endpoints at the
given distances from the axis' origin point.

    LineSegment3d.along Axis3d.x 3 5
    --> LineSegment3d.fromEndpoints
    -->     ( Point3d.fromCoordinates ( 3, 0, 0 )
    -->     , Point3d.fromCoordinates ( 5, 0, 0 )
    -->     )

    LineSegment3d.along Axis3d.y 2 -4
    --> LineSegment3d.fromEndpoints
    -->     ( Point3d.fromCoordinates ( 0, 2, 0 )
    -->     , Point3d.fromCoordinates ( 0, -4, 0 )
    -->     )

-}
along : Axis3d -> Float -> Float -> LineSegment3d
along axis start end =
    fromEndpoints ( Point3d.along axis start, Point3d.along axis end )


{-| Construct a 3D line segment lying _on_ a sketch plane by providing a 2D line
segment specified in XY coordinates _within_ the sketch plane.

    LineSegment3d.on SketchPlane3d.yz <|
        LineSegment2d.fromEndpoints
            ( Point2d.fromCoordinates ( 1, 2 )
            , Point2d.fromCoordinates ( 3, 4 )
            )
    --> LineSegment3d.fromEndpoints
    -->     ( Point3d.fromCoordinates ( 0, 1, 2 )
    -->     , Point3d.fromCoordinates ( 0, 3, 4 )
    -->     )

-}
on : SketchPlane3d -> LineSegment2d -> LineSegment3d
on sketchPlane lineSegment2d =
    let
        ( p1, p2 ) =
            LineSegment2d.endpoints lineSegment2d
    in
    fromEndpoints
        ( Point3d.on sketchPlane p1
        , Point3d.on sketchPlane p2
        )


{-| Get the start point of a line segment.

    LineSegment3d.startPoint exampleLineSegment
    --> Point3d.fromCoordinates ( 1, 2, 3 )

-}
startPoint : LineSegment3d -> Point3d
startPoint (Types.LineSegment3d ( start, _ )) =
    start


{-| Get the end point of a line segment.

    LineSegment3d.endPoint exampleLineSegment
    --> Point3d.fromCoordinates ( 4, 5, 6 )

-}
endPoint : LineSegment3d -> Point3d
endPoint (Types.LineSegment3d ( _, end )) =
    end


{-| Get the endpoints of a line segment as a tuple.

    ( p1, p2 ) =
        LineSegment3d.endpoints lineSegment

-}
endpoints : LineSegment3d -> ( Point3d, Point3d )
endpoints (Types.LineSegment3d endpoints_) =
    endpoints_


{-| Reverse a line segment, swapping its start and end points.

    LineSegment3d.reverse exampleLineSegment
    --> LineSegment3d.fromEndpoints
    -->     ( Point3d.fromCoordinates ( 4, 5, 6 )
    -->     , Point3d.fromCoordinates ( 1, 2, 3 )
    -->     )

-}
reverse : LineSegment3d -> LineSegment3d
reverse lineSegment =
    let
        ( p1, p2 ) =
            endpoints lineSegment
    in
    fromEndpoints ( p2, p1 )


{-| Get the midpoint of a line segment.

    LineSegment3d.midpoint exampleLineSegment
    --> Point3d.fromCoordinates ( 2.5, 3.5, 4.5 )

-}
midpoint : LineSegment3d -> Point3d
midpoint lineSegment =
    interpolate lineSegment 0.5


{-| Interpolate a line segment between its start and end points; a value of 0.0
corresponds to the start point of the line segment, a value of 0.5 corresponds
to its midpoint and a value of 1.0 corresponds to its end point. Values less
than 0.0 or greater than 1.0 can be used to extrapolate.

    LineSegment3d.interpolate exampleLineSegment (1 / 3)
    --> Point3d.fromCoordinates ( 2, 4, 5 )

    LineSegment3d.interpolate exampleLineSegment (-1 / 3)
    --> Point3d.fromCoordinates ( 0, 1, 2 )

-}
interpolate : LineSegment3d -> Float -> Point3d
interpolate lineSegment =
    let
        ( start, end ) =
            endpoints lineSegment
    in
    Point3d.interpolateFrom start end


{-| Get the length of a line segment.

    LineSegment3d.length exampleLineSegment
    --> 5.1962

-}
length : LineSegment3d -> Float
length =
    vector >> Vector3d.length


{-| Get the squared length of a line segment. Slightly more efficient than
`length` since it avoids a square root.

    LineSegment3d.squaredLength exampleLineSegment
    --> 27

-}
squaredLength : LineSegment3d -> Float
squaredLength =
    vector >> Vector3d.squaredLength


{-| Get the direction from a line segment's start point to its end point. If the
line segment has zero length (the start and end points are the same), returns
`Nothing`.

    LineSegment3d.direction exampleLineSegment
    --> Just
    -->     (Direction3d.fromAzimuthAndElevation
    -->         (degrees 45)
    -->         (degrees 35.26)
    -->     )

-}
direction : LineSegment3d -> Maybe Direction3d
direction =
    vector >> Vector3d.direction


{-| Get an arbitrary direction perpendicular to a line segment. If the line
segment has zero length, returns `Nothing`.

    LineSegment3d.perpendicularDirection exampleLineSegment
    --> Just
    -->     (Direction3d.fromAzimuthAndElevation
    -->         (degrees -90)
    -->         (degrees 45)
    -->     )

-}
perpendicularDirection : LineSegment3d -> Maybe Direction3d
perpendicularDirection =
    vector >> Vector3d.perpendicularTo >> Vector3d.direction


{-| Get the vector from a line segment's start point to its end point.

    LineSegment3d.vector exampleLineSegment
    --> Vector3d.fromComponents ( 2, 2, 2 )

-}
vector : LineSegment3d -> Vector3d
vector lineSegment =
    let
        ( p1, p2 ) =
            endpoints lineSegment
    in
    Vector3d.from p1 p2


{-| Scale a line segment about the given center point by the given scale.

    point =
        Point3d.fromCoordinates ( 1, 1, 1 )

    LineSegment3d.scaleAbout point 2 exampleLineSegment
    --> LineSegment3d.fromEndpoints
    -->     ( Point3d.fromCoordinates ( 1, 3, 5 )
    -->     , Point3d.fromCoordinates ( 7, 9, 11 )
    -->     )

-}
scaleAbout : Point3d -> Float -> LineSegment3d -> LineSegment3d
scaleAbout point scale =
    mapEndpoints (Point3d.scaleAbout point scale)


{-| Rotate a line segment around a given axis by a given angle (in radians).

    exampleLineSegment
        |> LineSegment3d.rotateAround Axis3d.z (degrees 90)
    --> LineSegment3d.fromEndpoints
    -->     ( Point3d.fromCoordinates ( -2, 1, 3 )
    -->     , Point3d.fromCoordinates ( -5, 4, 6 )
    -->     )

-}
rotateAround : Axis3d -> Float -> LineSegment3d -> LineSegment3d
rotateAround axis angle =
    mapEndpoints (Point3d.rotateAround axis angle)


{-| Translate a line segment by a given displacement.

    displacement =
        Vector3d.fromComponents ( 1, 2, 3 )

    exampleLineSegment
        |> LineSegment3d.translateBy displacement
    --> LineSegment3d.fromEndpoints
    -->     ( Point3d.fromCoordinates ( 2, 4, 6 )
    -->     , Point3d.fromCoordinates ( 5, 7, 9 )
    -->     )

-}
translateBy : Vector3d -> LineSegment3d -> LineSegment3d
translateBy displacementVector =
    mapEndpoints (Point3d.translateBy displacementVector)


{-| Translate a line segment in a given direction by a given distance;

    LineSegment3d.translateIn direction distance

is equivalent to

    LineSegment3d.translateBy
        (Vector3d.withLength distance direction)

-}
translateIn : Direction3d -> Float -> LineSegment3d -> LineSegment3d
translateIn translationDirection distance lineSegment =
    translateBy (Vector3d.withLength distance translationDirection) lineSegment


{-| Mirror a line segment across a plane.

    exampleLineSegment
        |> LineSegment3d.mirrorAcross Plane3d.xy
    --> LineSegment3d.fromEndpoints
    -->     ( Point3d.fromCoordinates ( 1, 2, -3 )
    -->     , Point3d.fromCoordinates ( 4, 5, -6 )
    -->     )

-}
mirrorAcross : Plane3d -> LineSegment3d -> LineSegment3d
mirrorAcross plane =
    mapEndpoints (Point3d.mirrorAcross plane)


{-| Find the [orthographic projection](https://en.wikipedia.org/wiki/Orthographic_projection)
of a line segment onto a plane.

    LineSegment3d.projectOnto Plane3d.yz exampleLineSegment
    --> LineSegment3d.fromEndpoints
    -->     ( Point3d.fromCoordinates ( 0, 2, 3 )
    -->     , Point3d.fromCoordinates ( 0, 5, 6 )
    -->     )

-}
projectOnto : Plane3d -> LineSegment3d -> LineSegment3d
projectOnto plane =
    mapEndpoints (Point3d.projectOnto plane)


{-| Transform the start and end points of a line segment by a given function
and create a new line segment from the resulting points. Most other
transformation functions can be defined in terms of `mapEndpoints`; for example,

    LineSegment3d.projectOnto plane

is equivalent to

    LineSegment3d.mapEndpoints (Point3d.projectOnto plane)

-}
mapEndpoints : (Point3d -> Point3d) -> LineSegment3d -> LineSegment3d
mapEndpoints function lineSegment =
    let
        ( p1, p2 ) =
            endpoints lineSegment
    in
    fromEndpoints ( function p1, function p2 )


{-| Take a line segment defined in global coordinates, and return it expressed
in local coordinates relative to a given reference frame.

    localFrame =
        Frame3d.atPoint
            (Point3d.fromCoordinates ( 1, 2, 3 ))

    LineSegment3d.relativeTo localFrame exampleLineSegment
    --> LineSegment3d.fromEndpoints
    -->     ( Point3d.fromCoordinates ( 0, 0, 0 )
    -->     , Point3d.fromCoordinates ( 3, 3, 3 )
    -->     )

-}
relativeTo : Frame3d -> LineSegment3d -> LineSegment3d
relativeTo frame =
    mapEndpoints (Point3d.relativeTo frame)


{-| Take a line segment considered to be defined in local coordinates relative
to a given reference frame, and return that line segment expressed in global
coordinates.

    localFrame =
        Frame3d.atPoint
            (Point3d.fromCoordinates ( 1, 2, 3 ))

    LineSegment3d.placeIn localFrame exampleLineSegment
    --> LineSegment3d.fromEndpoints
    -->     ( Point3d.fromCoordinates ( 2, 4, 6 )
    -->     , Point3d.fromCoordinates ( 5, 7, 9 )
    -->     )

-}
placeIn : Frame3d -> LineSegment3d -> LineSegment3d
placeIn frame =
    mapEndpoints (Point3d.placeIn frame)


{-| Project a line segment into a given sketch plane. Conceptually, this finds
the [orthographic projection](https://en.wikipedia.org/wiki/Orthographic_projection)
of the line segment onto the plane and then expresses the projected line segment
in 2D sketch coordinates.

    exampleLineSegment
        |> LineSegment3d.projectInto SketchPlane3d.xy
    --> LineSegment2d.fromEndpoints
    -->     ( Point2d.fromCoordinates ( 1, 2 )
    -->     , Point2d.fromCoordinates ( 4, 5 )
    -->     )

    exampleLineSegment
        |> LineSegment3d.projectInto SketchPlane3d.yz
    --> LineSegment2d.fromEndpoints
    -->     ( Point2d.fromCoordinates ( 2, 3 )
    -->     , Point2d.fromCoordinates ( 5, 6 )
    -->     )

    exampleLineSegment
        |> LineSegment3d.projectInto SketchPlane3d.zx
    --> LineSegment2d.fromEndpoints
    -->     ( Point2d.fromCoordinates ( 3, 1 )
    -->     , Point2d.fromCoordinates ( 6, 4 )
    -->     )

-}
projectInto : SketchPlane3d -> LineSegment3d -> LineSegment2d
projectInto sketchPlane lineSegment =
    let
        ( p1, p2 ) =
            endpoints lineSegment

        project =
            Point3d.projectInto sketchPlane
    in
    LineSegment2d.fromEndpoints ( project p1, project p2 )


{-| Get the minimal bounding box containing a line segment.

    LineSegment3d.boundingBox exampleLineSegment
    --> BoundingBox3d.fromExtrema
    -->     { minX = 1
    -->     , maxX = 4
    -->     , minY = 2
    -->     , maxY = 5
    -->     , minZ = 3
    -->     , maxZ = 6
    -->     }

-}
boundingBox : LineSegment3d -> BoundingBox3d
boundingBox lineSegment =
    let
        ( p1, p2 ) =
            endpoints lineSegment
    in
    BoundingBox3d.from p1 p2
