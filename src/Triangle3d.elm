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


{-| -}
fromVertices : ( Point3d, Point3d, Point3d ) -> Triangle3d
fromVertices =
    Types.Triangle3d


{-| -}
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


{-| -}
vertices : Triangle3d -> ( Point3d, Point3d, Point3d )
vertices (Types.Triangle3d vertices_) =
    vertices_


{-| -}
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


{-| -}
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


{-| -}
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


{-| -}
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


{-| -}
scaleAbout : Point3d -> Float -> Triangle3d -> Triangle3d
scaleAbout centerPoint scale =
    mapVertices (Point3d.scaleAbout centerPoint scale)


{-| -}
rotateAround : Axis3d -> Float -> Triangle3d -> Triangle3d
rotateAround axis angle =
    mapVertices (Point3d.rotateAround axis angle)


{-| -}
translateBy : Vector3d -> Triangle3d -> Triangle3d
translateBy vector =
    mapVertices (Point3d.translateBy vector)


{-| -}
translateIn : Direction3d -> Float -> Triangle3d -> Triangle3d
translateIn direction distance triangle =
    translateBy (Vector3d.withLength distance direction) triangle


{-| -}
mirrorAcross : Plane3d -> Triangle3d -> Triangle3d
mirrorAcross plane =
    mapVertices (Point3d.mirrorAcross plane)


{-| -}
projectOnto : Plane3d -> Triangle3d -> Triangle3d
projectOnto plane =
    mapVertices (Point3d.projectOnto plane)


{-| -}
mapVertices : (Point3d -> Point3d) -> Triangle3d -> Triangle3d
mapVertices function triangle =
    let
        ( p1, p2, p3 ) =
            vertices triangle
    in
    fromVertices ( function p1, function p2, function p3 )


{-| -}
relativeTo : Frame3d -> Triangle3d -> Triangle3d
relativeTo frame =
    mapVertices (Point3d.relativeTo frame)


{-| -}
placeIn : Frame3d -> Triangle3d -> Triangle3d
placeIn frame =
    mapVertices (Point3d.placeIn frame)


{-| -}
projectInto : SketchPlane3d -> Triangle3d -> Triangle2d
projectInto sketchPlane triangle =
    let
        ( p1, p2, p3 ) =
            vertices triangle

        project =
            Point3d.projectInto sketchPlane
    in
    Triangle2d.fromVertices ( project p1, project p2, project p3 )


{-| -}
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


{-| -}
circumcircle : Triangle3d -> Maybe Circle3d
circumcircle triangle =
    let
        ( p1, p2, p3 ) =
            vertices triangle
    in
    Circle3d.throughPoints p1 p2 p3
