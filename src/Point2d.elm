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


module Point2d
    exposing
        ( Point2d
        , along
        , circumcenter
        , coordinates
        , distanceFrom
        , equalWithin
        , fromCoordinates
        , fromCoordinatesIn
        , fromPolarCoordinates
        , fromPolarCoordinatesIn
        , interpolateFrom
        , midpoint
        , mirrorAcross
        , origin
        , placeIn
        , polarCoordinates
        , projectOnto
        , relativeTo
        , rotateAround
        , scaleAbout
        , signedDistanceAlong
        , signedDistanceFrom
        , squaredDistanceFrom
        , translateBy
        , translateIn
        , xCoordinate
        , yCoordinate
        )

{-| <img src="https://ianmackenzie.github.io/elm-geometry/1.0.0/Point2d/icon.svg" alt="Point2d" width="160">

A `Point2d` represents a position in 2D space and is defined by its X and Y
coordinates. This module contains a variety of point-related functionality, such
as

  - Measuring distance between points
  - Scaling, rotating, translating, mirroring and projecting points
  - Converting points between different coordinate systems

Points are distinct from vectors but interact with them in well-defined ways;
you can translate a point by a vector to result in a new point, or you can
compute the vector from one point to another, but you cannot 'add' two points
like you can add two vectors.

@docs Point2d


# Constants

@docs origin


# Constructors

@docs fromCoordinates, fromCoordinatesIn, fromPolarCoordinates, fromPolarCoordinatesIn, midpoint, interpolateFrom, along, circumcenter


# Properties

@docs coordinates, xCoordinate, yCoordinate, polarCoordinates


# Comparison

@docs equalWithin


# Measurement

@docs distanceFrom, squaredDistanceFrom, signedDistanceAlong, signedDistanceFrom


# Transformations

@docs scaleAbout, rotateAround, translateBy, translateIn, mirrorAcross, projectOnto


# Coordinate conversions

@docs relativeTo, placeIn

-}

import Bootstrap.Axis2d as Axis2d
import Bootstrap.Frame2d as Frame2d
import Direction2d exposing (Direction2d)
import Float.Extra as Float
import Geometry.Types as Types exposing (Axis2d, Frame2d)
import Vector2d exposing (Vector2d)


addTo : Point2d -> Vector2d -> Point2d
addTo point vector =
    translateBy vector point


{-| -}
type alias Point2d =
    Types.Point2d


{-| -}
origin : Point2d
origin =
    fromCoordinates ( 0, 0 )


{-| -}
fromCoordinates : ( Float, Float ) -> Point2d
fromCoordinates =
    Types.Point2d


{-| -}
fromPolarCoordinates : ( Float, Float ) -> Point2d
fromPolarCoordinates polarCoordinates_ =
    fromCoordinates (fromPolar polarCoordinates_)


{-| -}
midpoint : Point2d -> Point2d -> Point2d
midpoint firstPoint secondPoint =
    interpolateFrom firstPoint secondPoint 0.5


{-| -}
interpolateFrom : Point2d -> Point2d -> Float -> Point2d
interpolateFrom p1 p2 t =
    let
        ( x1, y1 ) =
            coordinates p1

        ( x2, y2 ) =
            coordinates p2
    in
    fromCoordinates
        ( Float.interpolateFrom x1 x2 t
        , Float.interpolateFrom y1 y2 t
        )


{-| -}
along : Axis2d -> Float -> Point2d
along axis distance =
    Axis2d.originPoint axis
        |> translateBy (Vector2d.withLength distance (Axis2d.direction axis))


{-| -}
fromCoordinatesIn : Frame2d -> ( Float, Float ) -> Point2d
fromCoordinatesIn frame localCoordinates =
    placeIn frame (fromCoordinates localCoordinates)


{-| -}
fromPolarCoordinatesIn : Frame2d -> ( Float, Float ) -> Point2d
fromPolarCoordinatesIn frame polarCoordinates_ =
    placeIn frame (fromPolarCoordinates polarCoordinates_)


{-| -}
circumcenter : Point2d -> Point2d -> Point2d -> Maybe Point2d
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

            ( x1, y1 ) =
                coordinates p1

            ( x2, y2 ) =
                coordinates p2

            ( x3, y3 ) =
                coordinates p3
        in
        Just <|
            fromCoordinates
                ( w1 * x3 + w2 * x1 + w3 * x2
                , w1 * y3 + w2 * y1 + w3 * y2
                )


{-| -}
coordinates : Point2d -> ( Float, Float )
coordinates (Types.Point2d coordinates_) =
    coordinates_


{-| -}
xCoordinate : Point2d -> Float
xCoordinate (Types.Point2d ( x, _ )) =
    x


{-| -}
yCoordinate : Point2d -> Float
yCoordinate (Types.Point2d ( _, y )) =
    y


{-| -}
polarCoordinates : Point2d -> ( Float, Float )
polarCoordinates point =
    toPolar (coordinates point)


{-| -}
equalWithin : Float -> Point2d -> Point2d -> Bool
equalWithin tolerance firstPoint secondPoint =
    squaredDistanceFrom firstPoint secondPoint <= tolerance * tolerance


{-| -}
distanceFrom : Point2d -> Point2d -> Float
distanceFrom firstPoint secondPoint =
    sqrt (squaredDistanceFrom firstPoint secondPoint)


{-| -}
squaredDistanceFrom : Point2d -> Point2d -> Float
squaredDistanceFrom firstPoint secondPoint =
    Vector2d.squaredLength (Vector2d.from firstPoint secondPoint)


{-| -}
signedDistanceAlong : Axis2d -> Point2d -> Float
signedDistanceAlong axis point =
    Vector2d.from (Axis2d.originPoint axis) point
        |> Vector2d.componentIn (Axis2d.direction axis)


{-| -}
signedDistanceFrom : Axis2d -> Point2d -> Float
signedDistanceFrom axis point =
    let
        directionVector =
            Direction2d.toVector (Axis2d.direction axis)

        displacementVector =
            Vector2d.from (Axis2d.originPoint axis) point
    in
    Vector2d.crossProduct directionVector displacementVector


{-| -}
scaleAbout : Point2d -> Float -> Point2d -> Point2d
scaleAbout centerPoint scale point =
    Vector2d.from centerPoint point
        |> Vector2d.scaleBy scale
        |> addTo centerPoint


{-| -}
rotateAround : Point2d -> Float -> Point2d -> Point2d
rotateAround centerPoint angle =
    Vector2d.from centerPoint >> Vector2d.rotateBy angle >> addTo centerPoint


{-| -}
translateBy : Vector2d -> Point2d -> Point2d
translateBy vector point =
    let
        ( vx, vy ) =
            Vector2d.components vector

        ( px, py ) =
            coordinates point
    in
    fromCoordinates ( px + vx, py + vy )


{-| -}
translateIn : Direction2d -> Float -> Point2d -> Point2d
translateIn direction distance point =
    let
        ( dx, dy ) =
            Direction2d.components direction

        ( px, py ) =
            coordinates point
    in
    fromCoordinates ( px + distance * dx, py + distance * dy )


{-| -}
mirrorAcross : Axis2d -> Point2d -> Point2d
mirrorAcross axis =
    Vector2d.from (Axis2d.originPoint axis)
        >> Vector2d.mirrorAcross axis
        >> addTo (Axis2d.originPoint axis)


{-| -}
projectOnto : Axis2d -> Point2d -> Point2d
projectOnto axis =
    Vector2d.from (Axis2d.originPoint axis)
        >> Vector2d.projectOnto axis
        >> addTo (Axis2d.originPoint axis)


{-| -}
relativeTo : Frame2d -> Point2d -> Point2d
relativeTo frame point =
    Vector2d.from (Frame2d.originPoint frame) point
        |> Vector2d.relativeTo frame
        |> Vector2d.components
        |> fromCoordinates


{-| -}
placeIn : Frame2d -> Point2d -> Point2d
placeIn frame point =
    Vector2d.fromComponents (coordinates point)
        |> Vector2d.placeIn frame
        |> addTo (Frame2d.originPoint frame)
