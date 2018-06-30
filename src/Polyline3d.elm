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


module Polyline3d
    exposing
        ( Polyline3d
        , boundingBox
        , fromVertices
        , length
        , mapVertices
        , mirrorAcross
        , on
        , placeIn
        , projectInto
        , projectOnto
        , relativeTo
        , rotateAround
        , scaleAbout
        , segments
        , translateBy
        , translateIn
        , vertices
        )

{-| <img src="https://ianmackenzie.github.io/elm-geometry/1.0.0/Polyline3d/icon.svg" alt="Polyline3d" width="160">

A `Polyline3d` represents a sequence of vertices in 3D connected by line
segments. This module contains a variety of polyline-related functionality, such
as

  - Computing the length of polylines
  - Scaling, rotating, translating and mirroring polylines
  - Converting polylines between different coordinate systems

@docs Polyline3d


# Constructors

@docs fromVertices, on


# Properties

@docs vertices, segments, length, boundingBox


# Transformations

Transforming a polyline is equivalent to transforming each of its vertices.

@docs scaleAbout, rotateAround, translateBy, translateIn, mirrorAcross, projectOnto, mapVertices


# Coordinate conversions

@docs relativeTo, placeIn, projectInto

-}

import Axis3d exposing (Axis3d)
import BoundingBox3d exposing (BoundingBox3d)
import Direction3d exposing (Direction3d)
import Frame3d exposing (Frame3d)
import Geometry.Types as Types
import LineSegment3d exposing (LineSegment3d)
import Plane3d exposing (Plane3d)
import Point3d exposing (Point3d)
import Polyline2d exposing (Polyline2d)
import SketchPlane3d exposing (SketchPlane3d)
import Vector3d exposing (Vector3d)


{-| -}
type alias Polyline3d =
    Types.Polyline3d


{-| Construct a polyline from its vertices:

    examplePolyline =
        Polyline3d.fromVertices
            [ Point2d.fromCoordinates ( 0, 0, 0 )
            , Point2d.fromCoordinates ( 1, 0, 0 )
            , Point2d.fromCoordinates ( 1, 2, 0 )
            , Point2d.fromCoordinates ( 1, 2, 3 )
            ]

-}
fromVertices : List Point3d -> Polyline3d
fromVertices =
    Types.Polyline3d


{-| Construct a 3D polyline lying _on_ a sketch plane by providing a 2D polyline
specified in XY coordinates _within_ the sketch plane.

    Polyline3d.on SketchPlane3d.yz <|
        Polyline2d.fromVertices
            [ Point2d.fromCoordinates ( 0, 0 )
            , Point2d.fromCoordinates ( 1, 0 )
            , Point2d.fromCoordinates ( 1, 1 )
            , Point2d.fromCoordinates ( 2, 1 )
            ]
    --> Polyline3d.fromVertices
    -->     [ Point3d.fromCoordinates ( 0, 0, 0 )
    -->     , Point3d.fromCoordinates ( 0, 1, 0 )
    -->     , Point3d.fromCoordinates ( 0, 1, 1 )
    -->     , Point3d.fromCoordinates ( 0, 2, 1 )
    -->     ]

-}
on : SketchPlane3d -> Polyline2d -> Polyline3d
on sketchPlane =
    Polyline2d.vertices >> List.map (Point3d.on sketchPlane) >> fromVertices


{-| Get the vertices of a polyline.

    Polyline3d.vertices examplePolyline
    --> [ Point3d.fromCoordinates ( 0, 0, 0 )
    --> , Point3d.fromCoordinates ( 1, 0, 0 )
    --> , Point3d.fromCoordinates ( 1, 2, 0 )
    --> , Point3d.fromCoordinates ( 1, 2, 3 )
    --> ]

-}
vertices : Polyline3d -> List Point3d
vertices (Types.Polyline3d vertices_) =
    vertices_


{-| Get the individual segments of a polyline.

    Polyline3d.segments examplePolyline
    --> [ LineSegment3d.fromEndpoints
    -->     ( Point3d.fromCoordinates ( 0, 0, 0 )
    -->     , Point3d.fromCoordinates ( 1, 0, 0 )
    -->     )
    --> , LineSegment3d.fromEndpoints
    -->     ( Point3d.fromCoordinates ( 1, 0, 0 )
    -->     , Point3d.fromCoordinates ( 1, 2, 0 )
    -->     )
    --> , LineSegment3d.fromEndpoints
    -->     ( Point3d.fromCoordinates ( 1, 2, 0 )
    -->     , Point3d.fromCoordinates ( 1, 2, 3 )
    -->     )
    --> ]

-}
segments : Polyline3d -> List LineSegment3d
segments polyline =
    case vertices polyline of
        [] ->
            []

        (first :: rest) as all ->
            List.map2 LineSegment3d.from all rest


{-| Get the overall length of a polyline (the sum of the lengths of its
segments).

    Polyline3d.length examplePolyline
    --> 6

-}
length : Polyline3d -> Float
length =
    segments >> List.map LineSegment3d.length >> List.sum


{-| Scale a polyline about the given center point by the given scale.

    point =
        Point3d.fromCoordinates ( 1, 0, 0 )

    Polyline3d.scaleAbout point 2 examplePolyline
    --> Polyline3d.fromVertices
    -->     [ Point3d.fromCoordinates ( -1, 0, 0 )
    -->     , Point3d.fromCoordinates ( 1, 0, 0 )
    -->     , Point3d.fromCoordinates ( 1, 4, 0 )
    -->     , Point3d.fromCoordinates ( 1, 4, 6 )
    -->     ]

-}
scaleAbout : Point3d -> Float -> Polyline3d -> Polyline3d
scaleAbout point scale =
    mapVertices (Point3d.scaleAbout point scale)


{-| Rotate a polyline around the given axis by the given angle (in radians).

    examplePolyline
        |> Polyline3d.rotateAround Axis3d.z (degrees 90)
    --> Polyline3d.fromVertices
    -->     [ Point3d.fromCoordinates ( 0, 0, 0 )
    -->     , Point3d.fromCoordinates ( 0, 1, 0 )
    -->     , Point3d.fromCoordinates ( -2, 1, 0 )
    -->     , Point3d.fromCoordinates ( -2, 1, 3 )
    -->     ]

-}
rotateAround : Axis3d -> Float -> Polyline3d -> Polyline3d
rotateAround axis angle =
    mapVertices (Point3d.rotateAround axis angle)


{-| Translate a polyline by the given displacement.

    displacement =
        Vector3d.fromComponents ( 1, 2, 3 )

    Polyline3d.translateBy displacement examplePolyline
    --> Polyline3d.fromVertices
    -->     [ Point3d.fromCoordinates ( 1, 2, 3 )
    -->     , Point3d.fromCoordinates ( 2, 2, 3 )
    -->     , Point3d.fromCoordinates ( 2, 4, 3 )
    -->     , Point3d.fromCoordinates ( 2, 4, 6 )
    -->     ]

-}
translateBy : Vector3d -> Polyline3d -> Polyline3d
translateBy vector =
    mapVertices (Point3d.translateBy vector)


{-| Translate a polyline in a given direction by a given distance;

    Polyline3d.translateIn direction distance

is equivalent to

    Polyline3d.translateBy
        (Vector3d.withLength distance direction)

-}
translateIn : Direction3d -> Float -> Polyline3d -> Polyline3d
translateIn direction distance polyline =
    translateBy (Vector3d.withLength distance direction) polyline


{-| Mirror a polyline across the given plane.

    Polyline3d.mirrorAcross Plane3d.xz examplePolyline
    --> Polyline3d.fromVertices
    -->     [ Point3d.fromCoordinates ( 0, 0, 0 )
    -->     , Point3d.fromCoordinates ( 1, 0, 0 )
    -->     , Point3d.fromCoordinates ( 1, -2, 0 )
    -->     , Point3d.fromCoordinates ( 1, -2, 3 )
    -->     ]

-}
mirrorAcross : Plane3d -> Polyline3d -> Polyline3d
mirrorAcross plane =
    mapVertices (Point3d.mirrorAcross plane)


{-| Find the [orthographic projection](https://en.wikipedia.org/wiki/Orthographic_projection)
of a polyline onto a plane. This will flatten the polyline.

    Polyline3d.projectOnto Plane3d.xz examplePolyline
    --> Polyline3d.fromVertices
    -->     [ Point3d.fromCoordinates ( 0, 0, 0 )
    -->     , Point3d.fromCoordinates ( 1, 0, 0 )
    -->     , Point3d.fromCoordinates ( 1, 0, 0 )
    -->     , Point3d.fromCoordinates ( 1, 0, 3 )
    -->     ]

-}
projectOnto : Plane3d -> Polyline3d -> Polyline3d
projectOnto plane =
    mapVertices (Point3d.projectOnto plane)


{-| Transform each vertex of a polyline by the given function. All other
transformations can be defined in terms of `mapVertices`; for example,

    Polyline3d.mirrorAcross plane

is equivalent to

    Polyline3d.mapVertices (Point3d.mirrorAcross plane)

-}
mapVertices : (Point3d -> Point3d) -> Polyline3d -> Polyline3d
mapVertices function =
    vertices >> List.map function >> fromVertices


{-| Take a polyline defined in global coordinates, and return it expressed
in local coordinates relative to a given reference frame.

    localFrame =
        Frame3d.atPoint
            (Point3d.fromCoordinates ( 1, 2, 3 ))

    Polyline3d.relativeTo localFrame examplePolyline
    --> Polyline3d.fromVertices
    -->     [ Point3d.fromCoordinates ( -1, -2, -3 )
    -->     , Point3d.fromCoordinates ( 0, -2, -3 )
    -->     , Point3d.fromCoordinates ( 0, 0, -3 )
    -->     , Point3d.fromCoordinates ( 0, 0, 0 )
    -->     ]

-}
relativeTo : Frame3d -> Polyline3d -> Polyline3d
relativeTo frame =
    mapVertices (Point3d.relativeTo frame)


{-| Take a polyline considered to be defined in local coordinates relative
to a given reference frame, and return that polyline expressed in global
coordinates.

    localFrame =
        Frame3d.atPoint
            (Point3d.fromCoordinates ( 1, 2, 3 ))

    Polyline3d.placeIn localFrame examplePolyline
    --> Polyline3d.fromVertices
    -->     [ Point3d.fromCoordinates ( 1, 2, 3 )
    -->     , Point3d.fromCoordinates ( 2, 2, 3 )
    -->     , Point3d.fromCoordinates ( 2, 4, 3 )
    -->     , Point3d.fromCoordinates ( 2, 4, 6 )
    -->     ]

-}
placeIn : Frame3d -> Polyline3d -> Polyline3d
placeIn frame =
    mapVertices (Point3d.placeIn frame)


{-| Project a polyline into a given sketch plane. Conceptually, this finds the
[orthographic projection](https://en.wikipedia.org/wiki/Orthographic_projection)
of the polyline onto the plane and then expresses the projected polyline in 2D
sketch coordinates.

    Polyline3d.projectInto Plane3d.xy examplePolyline
    --> Polyline2d.fromVertices
    -->     [ Point2d.fromCoordinates ( 0, 0 )
    -->     , Point2d.fromCoordinates ( 1, 0 )
    -->     , Point2d.fromCoordinates ( 1, 2 )
    -->     , Point2d.fromCoordinates ( 1, 2 )
    -->     ]

-}
projectInto : SketchPlane3d -> Polyline3d -> Polyline2d
projectInto sketchPlane =
    vertices
        >> List.map (Point3d.projectInto sketchPlane)
        >> Polyline2d.fromVertices


{-| Get the minimal bounding box containing a given polyline. Returns `Nothing`
if the polyline has no vertices.

    Polyline3d.boundingBox examplePolyline
    --> Just
    -->     (BoundingBox3d.fromExtrema
    -->         { minX = 0
    -->         , maxX = 1
    -->         , minY = 0
    -->         , maxY = 2
    -->         , minZ = 0
    -->         , maxZ = 3
    -->         }
    -->     )

-}
boundingBox : Polyline3d -> Maybe BoundingBox3d
boundingBox polyline =
    BoundingBox3d.containingPoints (vertices polyline)
