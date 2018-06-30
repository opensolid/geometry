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


module Triangle3d
    exposing
        ( Triangle3d
        , area
        , boundingBox
        , centroid
        , circumcircle
        , edges
        , fromVertices
        , mapVertices
        , mirrorAcross
        , normalDirection
        , on
        , placeIn
        , projectInto
        , projectOnto
        , relativeTo
        , rotateAround
        , scaleAbout
        , translateBy
        , translateIn
        , vertices
        )

{-| <img src="https://ianmackenzie.github.io/elm-geometry/1.0.0/Triangle3d/icon.svg" alt="Triangle3d" width="160">

A `Triangle3d` represents a triangle in 3D space, and is defined by its three
vertices. This module contains triangle-related functionality such as:

  - Finding the area and centroid of triangles
  - Scaling, rotating, translating, mirroring and projecting triangles
  - Converting triangles between different coordinate systems

@docs Triangle3d


# Constructors

@docs fromVertices, on


# Properties

@docs vertices, edges, centroid, area, normalDirection, boundingBox, circumcircle


# Transformations

Transforming a triangle is equivalent to transforming its vertices.

@docs scaleAbout, rotateAround, translateBy, translateIn, mirrorAcross, projectOnto, mapVertices


# Coordinate conversions

Functions for transforming triangles between local and global coordinates in
different coordinate frames.

@docs relativeTo, placeIn, projectInto

-}

import Axis3d exposing (Axis3d)
import BoundingBox3d exposing (BoundingBox3d)
import Circle3d exposing (Circle3d)
import Direction3d exposing (Direction3d)
import Frame3d exposing (Frame3d)
import Geometry.Types as Types
import LineSegment3d exposing (LineSegment3d)
import Plane3d exposing (Plane3d)
import Point3d exposing (Point3d)
import SketchPlane3d exposing (SketchPlane3d)
import Triangle2d exposing (Triangle2d)
import Vector3d exposing (Vector3d)


{-| -}
type alias Triangle3d =
    Types.Triangle3d


{-| Construct a triangle from its three vertices:

    exampleTriangle =
        Triangle3d.fromVertices
            ( Point3d.fromCoordinates ( 1, 0, 0 )
            , Point3d.fromCoordinates ( 2, 0, 0 )
            , Point3d.fromCoordinates ( 2, 1, 3 )
            )

-}
fromVertices : ( Point3d, Point3d, Point3d ) -> Triangle3d
fromVertices =
    Types.Triangle3d


{-| Construct a 3D triangle lying _on_ a sketch plane by providing a 2D triangle
specified in XY coordinates _within_ the sketch plane.

    Triangle3d.on SketchPlane3d.xz <|
        Triangle2d.fromVertices
            ( Point2d.fromCoordinates ( 1, 1 )
            , Point2d.fromCoordinates ( 2, 1 )
            , Point2d.fromCoordinates ( 1, 3 )
            )
    --> Triangle3d.fromVertices
    -->     ( Point3d.fromCoordinates ( 1, 0, 1 )
    -->     , Point3d.fromCoordinates ( 2, 0, 1 )
    -->     , Point3d.fromCoordinates ( 1, 0, 3 )
    -->     )

-}
on : SketchPlane3d -> Triangle2d -> Triangle3d
on sketchPlane triangle2d =
    let
        ( p1, p2, p3 ) =
            Triangle2d.vertices triangle2d
    in
    fromVertices
        ( Point3d.on sketchPlane p1
        , Point3d.on sketchPlane p2
        , Point3d.on sketchPlane p3
        )


{-| Get the vertices of a triangle.

    ( p1, p2, p3 ) =
        Triangle3d.vertices exampleTriangle


    --> p1 = Point3d.fromCoordinates ( 1, 0, 0 )
    --> p2 = Point3d.fromCoordinates ( 2, 0, 0 )
    --> p3 = Point3d.fromCoordinates ( 2, 1, 3 )

-}
vertices : Triangle3d -> ( Point3d, Point3d, Point3d )
vertices (Types.Triangle3d vertices_) =
    vertices_


{-| Get the edges of a triangle: from the first vertex to the second, from the
second to the third, and from the third back to the first.

    ( e1, e2, e3 ) =
        Triangle3d.edges exampleTriangle


    --> e1 =
    -->     LineSegment3d.fromEndpoints
    -->         ( Point3d.fromCoordinates ( 1, 0, 0 )
    -->         , Point3d.fromCoordinates ( 2, 0, 0 )
    -->         )
    -->
    --> e2 =
    -->     LineSegment3d.fromEndpoints
    -->         ( Point3d.fromCoordinates ( 2, 0, 0 )
    -->         , Point3d.fromCoordinates ( 2, 1, 3 )
    -->         )
    -->
    --> e3 =
    -->     LineSegment3d.fromEndpoints
    -->         ( Point3d.fromCoordinates ( 2, 1, 3 )
    -->         , Point3d.fromCoordinates ( 1, 0, 0 )
    -->         )

-}
edges : Triangle3d -> ( LineSegment3d, LineSegment3d, LineSegment3d )
edges triangle =
    let
        ( p1, p2, p3 ) =
            vertices triangle
    in
    ( LineSegment3d.from p1 p2
    , LineSegment3d.from p2 p3
    , LineSegment3d.from p3 p1
    )


{-| Get the centroid (center of mass) of a triangle.

    Triangle3d.centroid exampleTriangle
    --> Point3d.fromCoordinates ( 1.6667, 0.6667, 1 )

-}
centroid : Triangle3d -> Point3d
centroid triangle =
    let
        ( p1, p2, p3 ) =
            vertices triangle

        firstVector =
            Vector3d.from p1 p2

        secondVector =
            Vector3d.from p1 p3

        displacement =
            Vector3d.scaleBy (1 / 3) (Vector3d.sum firstVector secondVector)
    in
    Point3d.translateBy displacement p1


{-| Get the area of a triangle. This value is always positive.

    Triangle3d.area exampleTriangle
    --> 1.5811

-}
area : Triangle3d -> Float
area triangle =
    let
        ( p1, p2, p3 ) =
            vertices triangle

        firstVector =
            Vector3d.from p1 p2

        secondVector =
            Vector3d.from p1 p3
    in
    0.5 * Vector3d.length (Vector3d.crossProduct firstVector secondVector)


{-| Attempt to find the normal direction to a triangle. The resulting direction
will be oriented such that the triangle vertices are in counterclockwise order
around it according to the right-hand rule. If the triangle is degenerate (its
three vertices are collinear), returns `Nothing`.

    Triangle3d.normalDirection exampleTriangle
    --> Just
    -->     (Direction3d.fromAzimuthAndElevation
    -->         (degrees -90)
    -->         (degrees 18.43)
    -->     )

-}
normalDirection : Triangle3d -> Maybe Direction3d
normalDirection triangle =
    let
        ( p1, p2, p3 ) =
            vertices triangle

        v1 =
            Vector3d.from p1 p2

        v2 =
            Vector3d.from p2 p3
    in
    Vector3d.direction (Vector3d.crossProduct v1 v2)


{-| Scale a triangle about a given point by a given scale.

    Triangle3d.scaleAbout Point3d.origin 2 exampleTriangle
    --> Triangle3d.fromVertices
    -->     ( Point3d.fromCoordinates ( 2, 0, 0 )
    -->     , Point3d.fromCoordinates ( 4, 0, 0 )
    -->     , Point3d.fromCoordinates ( 4, 2, 6 )
    -->     )

-}
scaleAbout : Point3d -> Float -> Triangle3d -> Triangle3d
scaleAbout centerPoint scale =
    mapVertices (Point3d.scaleAbout centerPoint scale)


{-| Rotate a triangle around a given axis by a given angle (in radians).

    exampleTriangle
        |> Triangle3d.rotateAround Axis3d.z (degrees 90)
    --> Triangle3d.fromVertices
    -->     ( Point3d.fromCoordinates ( 0, 1, 0 )
    -->     , Point3d.fromCoordinates ( 0, 2, 0 )
    -->     , Point3d.fromCoordinates ( -1, 2, 3 )
    -->     )

-}
rotateAround : Axis3d -> Float -> Triangle3d -> Triangle3d
rotateAround axis angle =
    mapVertices (Point3d.rotateAround axis angle)


{-| Translate a triangle by a given displacement.

    displacement =
        Vector3d.fromComponents ( 2, -1, 3 )

    Triangle3d.translateBy displacement exampleTriangle
    --> Triangle3d.fromVertices
    -->     ( Point3d.fromCoordinates ( 3, -1, 3 )
    -->     , Point3d.fromCoordinates ( 4, -1, 3 )
    -->     , Point3d.fromCoordinates ( 4, 0, 6 )
    -->     )

-}
translateBy : Vector3d -> Triangle3d -> Triangle3d
translateBy vector =
    mapVertices (Point3d.translateBy vector)


{-| Translate a triangle in a given direction by a given distance;

    Triangle3d.translateIn direction distance

is equivalent to

    Triangle3d.translateBy
        (Vector3d.withLength distance direction)

-}
translateIn : Direction3d -> Float -> Triangle3d -> Triangle3d
translateIn direction distance triangle =
    translateBy (Vector3d.withLength distance direction) triangle


{-| Mirror a triangle across a given plane.

    Triangle3d.mirrorAcross Plane3d.yz exampleTriangle
    --> Triangle3d.fromVertices
    -->     ( Point3d.fromCoordinates ( -1, 0, 0 )
    -->     ( Point3d.fromCoordinates ( -2, 0, 0 )
    -->     ( Point3d.fromCoordinates ( -2, 1, 3 )
    -->     )

-}
mirrorAcross : Plane3d -> Triangle3d -> Triangle3d
mirrorAcross plane =
    mapVertices (Point3d.mirrorAcross plane)


{-| Find the [orthographic projection](https://en.wikipedia.org/wiki/Orthographic_projection)
of a triangle onto a plane.

    Triangle3d.projectOnto Plane3d.xy exampleTriangle
    --> Triangle3d.fromVertices
    -->     ( Point3d.fromCoordinates ( 1, 0, 0 )
    -->     , Point3d.fromCoordinates ( 2, 0, 0 )
    -->     , Point3d.fromCoordinates ( 2, 1, 0 )
    -->     )

    Triangle3d.projectOnto Plane3d.xz exampleTriangle
    --> Triangle3d.fromVertices
    -->     ( Point3d.fromCoordinates ( 1, 0, 0 )
    -->     , Point3d.fromCoordinates ( 2, 0, 0 )
    -->     , Point3d.fromCoordinates ( 2, 0, 3 )
    -->     )

-}
projectOnto : Plane3d -> Triangle3d -> Triangle3d
projectOnto plane =
    mapVertices (Point3d.projectOnto plane)


{-| Transform each vertex of a triangle by a given function and create a new
triangle from the resulting points. Most other transformation functions can be
defined in terms of `mapVertices`; for example,

    Triangle3d.projectOnto plane

is equivalent to

    Triangle3d.mapVertices (Point3d.projectOnto plane)

-}
mapVertices : (Point3d -> Point3d) -> Triangle3d -> Triangle3d
mapVertices function triangle =
    let
        ( p1, p2, p3 ) =
            vertices triangle
    in
    fromVertices ( function p1, function p2, function p3 )


{-| Take a triangle defined in global coordinates, and return it expressed
in local coordinates relative to a given reference frame.

    localFrame =
        Frame3d.atPoint
            (Point3d.fromCoordinates ( 2, 1, 3 ))

    Triangle3d.relativeTo localFrame exampleTriangle
    --> Triangle3d.fromVertices
    -->     ( Point3d.fromCoordinates ( -1, -1, -3 )
    -->     , Point3d.fromCoordinates ( 0, -1, -3 )
    -->     , Point3d.fromCoordinates ( 0, 0, 0 )
    -->     )

-}
relativeTo : Frame3d -> Triangle3d -> Triangle3d
relativeTo frame =
    mapVertices (Point3d.relativeTo frame)


{-| Take a triangle considered to be defined in local coordinates relative to a
given reference frame, and return that triangle expressed in global coordinates.

    localFrame =
        Frame3d.atPoint
            (Point3d.fromCoordinates ( 2, 1, 3 ))

    Triangle3d.placeIn localFrame exampleTriangle
    --> Triangle3d.fromVertices
    -->     ( Point3d.fromCoordinates ( 3, 1, 3 )
    -->     , Point3d.fromCoordinates ( 4, 1, 3 )
    -->     , Point3d.fromCoordinates ( 4, 2, 6 )
    -->     )

-}
placeIn : Frame3d -> Triangle3d -> Triangle3d
placeIn frame =
    mapVertices (Point3d.placeIn frame)


{-| Project a triangle into a given sketch plane. Conceptually, this finds the
[orthographic projection](https://en.wikipedia.org/wiki/Orthographic_projection)
of the triangle onto the plane and then expresses the projected triangle in 2D
sketch coordinates.

    Triangle3d.projectInto SketchPlane3d.xy exampleTriangle
    --> Triangle2d.fromVertices
    -->     ( Point2d.fromCoordinates ( 1, 0 )
    -->     , Point2d.fromCoordinates ( 2, 0 )
    -->     , Point2d.fromCoordinates ( 2, 1 )
    -->     )

    Triangle3d.projectInto SketchPlane3d.zx exampleTriangle
    --> Triangle2d.fromVertices
    -->     ( Point2d.fromCoordinates ( 0, 1 )
    -->     , Point2d.fromCoordinates ( 0, 2 )
    -->     , Point2d.fromCoordinates ( 3, 2 )
    -->     )

-}
projectInto : SketchPlane3d -> Triangle3d -> Triangle2d
projectInto sketchPlane triangle =
    let
        ( p1, p2, p3 ) =
            vertices triangle

        project =
            Point3d.projectInto sketchPlane
    in
    Triangle2d.fromVertices ( project p1, project p2, project p3 )


{-| Get the minimal bounding box containing a given triangle.

    Triangle3d.boundingBox exampleTriangle
    --> BoundingBox3d.fromExtrema
    -->     { minX = 1
    -->     , maxX = 2
    -->     , minY = 0
    -->     , maxY = 1
    -->     , minZ = 0
    -->     , maxZ = 3
    -->     }

-}
boundingBox : Triangle3d -> BoundingBox3d
boundingBox triangle =
    let
        ( p1, p2, p3 ) =
            vertices triangle

        ( x1, y1, z1 ) =
            Point3d.coordinates p1

        ( x2, y2, z2 ) =
            Point3d.coordinates p2

        ( x3, y3, z3 ) =
            Point3d.coordinates p3
    in
    BoundingBox3d.fromExtrema
        { minX = min x1 (min x2 x3)
        , maxX = max x1 (max x2 x3)
        , minY = min y1 (min y2 y3)
        , maxY = max y1 (max y2 y3)
        , minZ = min z1 (min z2 z3)
        , maxZ = max z1 (max z2 z3)
        }


{-| Attempt to find the circumcircle of a triangle, a circle that passes through
each of the triangle's vertices;

    Triangle3d.circumcircle triangle

is equivalent to

    ( p1, p2, p3 ) =
        Triangle3d.vertices triangle

    Circle3d.throughPoints p1 p2 p3

If the triangle is degenerate (its three vertices are collinear), returns
`Nothing`.

-}
circumcircle : Triangle3d -> Maybe Circle3d
circumcircle triangle =
    let
        ( p1, p2, p3 ) =
            vertices triangle
    in
    Circle3d.throughPoints p1 p2 p3
