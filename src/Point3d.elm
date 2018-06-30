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


{-| -}
origin : Point3d
origin =
    fromCoordinates ( 0, 0, 0 )


{-| -}
fromCoordinates : ( Float, Float, Float ) -> Point3d
fromCoordinates =
    Types.Point3d


{-| -}
midpoint : Point3d -> Point3d -> Point3d
midpoint firstPoint secondPoint =
    interpolateFrom firstPoint secondPoint 0.5


{-| -}
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


{-| -}
along : Axis3d -> Float -> Point3d
along (Types.Axis3d axis) distance =
    axis.originPoint
        |> translateBy (Vector3d.withLength distance axis.direction)


{-| -}
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


{-| -}
fromCoordinatesIn : Frame3d -> ( Float, Float, Float ) -> Point3d
fromCoordinatesIn frame localCoordinates =
    placeIn frame (fromCoordinates localCoordinates)


{-| -}
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


{-| -}
coordinates : Point3d -> ( Float, Float, Float )
coordinates (Types.Point3d coordinates_) =
    coordinates_


{-| -}
xCoordinate : Point3d -> Float
xCoordinate (Types.Point3d ( x, _, _ )) =
    x


{-| -}
yCoordinate : Point3d -> Float
yCoordinate (Types.Point3d ( _, y, _ )) =
    y


{-| -}
zCoordinate : Point3d -> Float
zCoordinate (Types.Point3d ( _, _, z )) =
    z


{-| -}
equalWithin : Float -> Point3d -> Point3d -> Bool
equalWithin tolerance firstPoint secondPoint =
    squaredDistanceFrom firstPoint secondPoint <= tolerance * tolerance


{-| -}
distanceFrom : Point3d -> Point3d -> Float
distanceFrom firstPoint secondPoint =
    sqrt (squaredDistanceFrom firstPoint secondPoint)


{-| -}
squaredDistanceFrom : Point3d -> Point3d -> Float
squaredDistanceFrom firstPoint secondPoint =
    Vector3d.squaredLength (Vector3d.from firstPoint secondPoint)


{-| -}
signedDistanceAlong : Axis3d -> Point3d -> Float
signedDistanceAlong (Types.Axis3d axis) point =
    Vector3d.from axis.originPoint point |> Vector3d.componentIn axis.direction


{-| -}
distanceFromAxis : Axis3d -> Point3d -> Float
distanceFromAxis axis point =
    sqrt (squaredDistanceFromAxis axis point)


{-| -}
squaredDistanceFromAxis : Axis3d -> Point3d -> Float
squaredDistanceFromAxis (Types.Axis3d axis) point =
    Vector3d.from axis.originPoint point
        |> Vector3d.crossProduct (Direction3d.toVector axis.direction)
        |> Vector3d.squaredLength


{-| -}
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


{-| -}
scaleAbout : Point3d -> Float -> Point3d -> Point3d
scaleAbout centerPoint scale point =
    Vector3d.from centerPoint point
        |> Vector3d.scaleBy scale
        |> addTo centerPoint


{-| -}
rotateAround : Axis3d -> Float -> Point3d -> Point3d
rotateAround ((Types.Axis3d axis) as axis_) angle point =
    Vector3d.from axis.originPoint point
        |> Vector3d.rotateAround axis_ angle
        |> addTo axis.originPoint


{-| -}
translateBy : Vector3d -> Point3d -> Point3d
translateBy vector point =
    let
        ( vx, vy, vz ) =
            Vector3d.components vector

        ( px, py, pz ) =
            coordinates point
    in
    fromCoordinates ( px + vx, py + vy, pz + vz )


{-| -}
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


{-| -}
mirrorAcross : Plane3d -> Point3d -> Point3d
mirrorAcross plane point =
    let
        originPoint =
            Plane3d.originPoint plane
    in
    Vector3d.from originPoint point
        |> Vector3d.mirrorAcross plane
        |> addTo originPoint


{-| -}
projectOnto : Plane3d -> Point3d -> Point3d
projectOnto plane point =
    let
        displacement =
            Vector3d.withLength -(signedDistanceFrom plane point)
                (Plane3d.normalDirection plane)
    in
    translateBy displacement point


{-| -}
projectOntoAxis : Axis3d -> Point3d -> Point3d
projectOntoAxis axis point =
    along axis (signedDistanceAlong axis point)


{-| -}
relativeTo : Frame3d -> Point3d -> Point3d
relativeTo frame point =
    Vector3d.from (Frame3d.originPoint frame) point
        |> Vector3d.relativeTo frame
        |> Vector3d.components
        |> fromCoordinates


{-| -}
placeIn : Frame3d -> Point3d -> Point3d
placeIn frame point =
    Vector3d.fromComponents (coordinates point)
        |> Vector3d.placeIn frame
        |> addTo (Frame3d.originPoint frame)


{-| -}
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
