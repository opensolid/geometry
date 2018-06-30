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


module Frame2d
    exposing
        ( Frame2d
        , atCoordinates
        , atPoint
        , isRightHanded
        , mirrorAcross
        , moveTo
        , originPoint
        , placeIn
        , relativeTo
        , reverseX
        , reverseY
        , rotateAround
        , rotateBy
        , translateAlongOwn
        , translateBy
        , translateIn
        , unsafe
        , withXDirection
        , withYDirection
        , xAxis
        , xDirection
        , xy
        , yAxis
        , yDirection
        )

{-| <img src="https://ianmackenzie.github.io/elm-geometry/1.0.0/Frame2d/icon.svg" alt="Frame2d" width="160">

A `Frame2d` has an origin point and a pair of X and Y directions (which are
always perpendicular to each other). It can be thought of as:

  - A local coordinate system: Most geometric types have associated `relativeTo`
    and `placeIn` functions that convert values of that type from global
    coordinates to local coordinates in a particular frame, and vice versa.
  - A pair of X and Y axes: It is often convenient to (for example) mirror
    across the X axis of a frame, or project onto its Y axis. Frames can
    also themselves be translated, rotated and mirrored!
  - A combined 2D position and orientation: For example, a `Frame2d` could be
    used to define the position and orientation of a spaceship in a 2D game.
    Movement of the ship would then be done by translating and rotating the
    frame.

@docs Frame2d


# Constants

@docs xy


# Constructors

@docs atPoint, atCoordinates, withXDirection, withYDirection, unsafe


# Properties

@docs originPoint, xDirection, yDirection, isRightHanded, xAxis, yAxis


# Transformations

@docs reverseX, reverseY, moveTo, rotateBy, rotateAround, translateBy, translateIn, translateAlongOwn, mirrorAcross


# Coordinate conversions

@docs relativeTo, placeIn

-}

import Axis2d exposing (Axis2d)
import Direction2d exposing (Direction2d)
import Geometry.Types as Types
import Point2d exposing (Point2d)
import Vector2d exposing (Vector2d)


{-| -}
type alias Frame2d =
    Types.Frame2d


{-| The global XY frame.

    Frame2d.originPoint Frame2d.xy
    --> Point2d.origin

    Frame2d.xDirection Frame2d.xy
    --> Direction2d.x

    Frame2d.yDirection Frame2d.xy
    --> Direction2d.y

-}
xy : Frame2d
xy =
    atPoint Point2d.origin


{-| Construct a frame with the given X axis direction, having the given origin
point. The Y axis direction will be constructed by rotating the given X
direction 90 degrees counterclockwise:

    frame =
        Frame2d.withXDirection
            (Direction2d.fromAngle (degrees 30))
            (Point2d.fromCoordinates ( 2, 3 ))

    Frame2d.yDirection frame
    --> Direction2d.fromAngle (degrees 120)

-}
withXDirection : Direction2d -> Point2d -> Frame2d
withXDirection xDirection_ originPoint_ =
    unsafe
        { originPoint = originPoint_
        , xDirection = xDirection_
        , yDirection = xDirection_ |> Direction2d.rotateCounterclockwise
        }


{-| Construct a frame with the given Y axis direction, having the given origin
point. The X axis direction will be constructed by rotating the given X
direction 90 degrees clockwise:

    frame =
        Frame2d.withYDirection
            (Direction2d.fromAngle (degrees 30))
            (Point2d.fromCoordinates ( 2, 3 ))

    Frame2d.yDirection frame
    --> Direction2d.fromAngle (degrees -60)

-}
withYDirection : Direction2d -> Point2d -> Frame2d
withYDirection yDirection_ originPoint_ =
    unsafe
        { originPoint = originPoint_
        , xDirection = yDirection_ |> Direction2d.rotateClockwise
        , yDirection = yDirection_
        }


{-| Construct a frame directly from its origin point and X and Y directions:

    frame =
        Frame2d.unsafe
            { originPoint =
                Point2d.fromCoordinates ( 2, 3 )
            , xDirection =
                Direction2d.fromAngle (degrees 45)
            , yDirection =
                Direction2d.fromAngle (degrees 135)
            }

In this case **you must be careful to ensure that the X and Y directions are
perpendicular**. To construct pairs of perpendicular directions,
[`Direction2d.orthonormalize`](Direction2d#orthonormalize) or
[`Direction2d.orthogonalize`](Direction2d#orthogonalize) may be useful.

-}
unsafe : { originPoint : Point2d, xDirection : Direction2d, yDirection : Direction2d } -> Frame2d
unsafe =
    Types.Frame2d


{-| Construct a frame aligned with the global XY frame but with the given origin
point.

    point =
        Point2d.fromCoordinates ( 2, 3 )

    frame =
        Frame2d.atPoint point

    Frame2d.originPoint frame
    --> point

    Frame2d.xDirection frame
    --> Direction2d.x

    Frame2d.yDirection frame
    --> Direction2d.y

-}
atPoint : Point2d -> Frame2d
atPoint point =
    unsafe
        { originPoint = point
        , xDirection = Direction2d.x
        , yDirection = Direction2d.y
        }


{-| Shorthand for `Frame2d.atPoint`;

    Frame2d.atCoordinates ( x, y )

is equivalent to

    Frame2d.atPoint (Point2d.fromCoordinates ( x, y ))

-}
atCoordinates : ( Float, Float ) -> Frame2d
atCoordinates coordinates =
    atPoint (Point2d.fromCoordinates coordinates)


{-| Get the origin point of a given frame.

    Frame2d.originPoint Frame2d.xy
    --> Point2d.origin

-}
originPoint : Frame2d -> Point2d
originPoint (Types.Frame2d frame) =
    frame.originPoint


{-| Get the X direction of a given frame.

    Frame2d.xDirection Frame2d.xy
    --> Direction2d.x

-}
xDirection : Frame2d -> Direction2d
xDirection (Types.Frame2d frame) =
    frame.xDirection


{-| Get the Y direction of a given frame.

    Frame2d.yDirection Frame2d.xy
    --> Direction2d.y

-}
yDirection : Frame2d -> Direction2d
yDirection (Types.Frame2d frame) =
    frame.yDirection


{-| Check if a frame is [right-handed](https://en.wikipedia.org/wiki/Cartesian_coordinate_system#Orientation_and_handedness).

    Frame2d.isRightHanded Frame2d.xy
    --> True

    Frame2d.isRightHanded (Frame2d.reverseX Frame2d.xy)
    --> False

All predefined frames are right-handed, and most operations on frames preserve
handedness, so about the only ways to end up with a left-handed frame are by
constructing one explicitly with `unsafe` or by mirroring a right-handed frame.

-}
isRightHanded : Frame2d -> Bool
isRightHanded frame =
    let
        xVector =
            Direction2d.toVector (xDirection frame)

        yVector =
            Direction2d.toVector (yDirection frame)
    in
    Vector2d.crossProduct xVector yVector > 0


{-| Get the X axis of a given frame (the axis formed from the frame's origin
point and X direction).

    Frame2d.xAxis Frame2d.xy
    --> Axis2d.x

-}
xAxis : Frame2d -> Axis2d
xAxis (Types.Frame2d frame) =
    Axis2d.through frame.originPoint frame.xDirection


{-| Get the Y axis of a given frame (the axis formed from the frame's origin
point and Y direction).

    Frame2d.yAxis Frame2d.xy
    --> Axis2d.y

-}
yAxis : Frame2d -> Axis2d
yAxis (Types.Frame2d frame) =
    Axis2d.through frame.originPoint frame.yDirection


{-| Reverse the X direction of a frame, leaving its Y direction and origin point
the same.

    point =
        Point2d.fromCoordinates ( 2, 3 )

    frame =
        Frame2d.atPoint point |> Frame2d.reverseX

    Frame2d.originPoint frame
    --> point

    Frame2d.xDirection frame
    --> Direction2d.negativeX

    Frame2d.yDirection frame
    --> Direction2d.y

Note that this will switch the [handedness](https://en.wikipedia.org/wiki/Cartesian_coordinate_system#Orientation_and_handedness)
of the frame.

-}
reverseX : Frame2d -> Frame2d
reverseX frame =
    unsafe
        { originPoint = originPoint frame
        , xDirection = Direction2d.reverse (xDirection frame)
        , yDirection = yDirection frame
        }


{-| Reverse the Y direction of a frame, leaving its X direction and origin point
the same.

    point =
        Point2d.fromCoordinates ( 2, 3 )

    frame =
        Frame2d.atPoint point |> Frame2d.reverseY

    Frame2d.originPoint frame
    --> point

    Frame2d.xDirection frame
    --> Direction2d.x

    Frame2d.yDirection frame
    --> Direction2d.negativeY

Note that this will switch the [handedness](https://en.wikipedia.org/wiki/Cartesian_coordinate_system#Orientation_and_handedness)
of the frame.

-}
reverseY : Frame2d -> Frame2d
reverseY frame =
    unsafe
        { originPoint = originPoint frame
        , xDirection = xDirection frame
        , yDirection = Direction2d.reverse (yDirection frame)
        }


{-| Move a frame so that it has the given origin point.

    point =
        Point2d.fromCoordinates ( 1, 1 )

    Frame2d.xy |> Frame2d.moveTo point
    --> Frame2d.atPoint point

-}
moveTo : Point2d -> Frame2d -> Frame2d
moveTo newOrigin frame =
    unsafe
        { originPoint = newOrigin
        , xDirection = xDirection frame
        , yDirection = yDirection frame
        }


{-| Rotate a frame counterclockwise by a given angle around the frame's own
origin point. The resulting frame will have the same origin point, and its X and
Y directions will be rotated by the given angle.

    rotatedFrame =
        Frame2d.rotateBy (degrees 30) Frame2d.xy

    Frame2d.xDirection rotatedFrame
    --> Direction2d.fromAngle (degrees 30)

    Frame2d.yDirection rotatedFrame
    --> Direction2d.fromAngle (degrees 120)

-}
rotateBy : Float -> Frame2d -> Frame2d
rotateBy angle frame =
    let
        rotateDirection =
            Direction2d.rotateBy angle
    in
    unsafe
        { originPoint = originPoint frame
        , xDirection = rotateDirection (xDirection frame)
        , yDirection = rotateDirection (yDirection frame)
        }


{-| Rotate a frame counterclockwise around a given point by a given angle. The
frame's origin point will be rotated around the given point by the given angle,
and its X and Y basis directions will be rotated by the given angle.

    rotatedFrame =
        Frame2d.atPoint (Point2d.fromCoordinates ( 1, 1 ))
            |> Frame2d.rotateAround Point2d.origin
                (degrees 45)

    Frame2d.originPoint rotatedFrame
    --> Point2d.fromCoordinates ( 0, 1.4142 )

    Frame2d.xDirection rotatedFrame
    --> Direction2d.fromAngle (degrees 45)

    Frame2d.yDirection rotatedFrame
    --> Direction2d.fromAngle (degrees 135)

-}
rotateAround : Point2d -> Float -> Frame2d -> Frame2d
rotateAround centerPoint angle =
    let
        rotatePoint =
            Point2d.rotateAround centerPoint angle

        rotateDirection =
            Direction2d.rotateBy angle
    in
    \frame ->
        unsafe
            { originPoint = rotatePoint (originPoint frame)
            , xDirection = rotateDirection (xDirection frame)
            , yDirection = rotateDirection (yDirection frame)
            }


{-| Translate a frame by a given displacement.

    frame =
        Frame2d.atPoint (Point2d.fromCoordinates ( 2, 3 ))

    displacement =
        Vector2d.fromComponents ( 1, 1 )

    Frame2d.translateBy displacement frame
    --> Frame2d.atPoint (Point2d.fromCoordinates ( 3, 4 ))

-}
translateBy : Vector2d -> Frame2d -> Frame2d
translateBy vector frame =
    unsafe
        { originPoint = Point2d.translateBy vector (originPoint frame)
        , xDirection = xDirection frame
        , yDirection = yDirection frame
        }


{-| Translate a frame in a given direction by a given distance;

    Frame2d.translateIn direction distance

is equivalent to

    Frame2d.translateBy
        (Vector2d.withLength distance direction)

-}
translateIn : Direction2d -> Float -> Frame2d -> Frame2d
translateIn direction distance frame =
    translateBy (Vector2d.withLength distance direction) frame


{-| Translate a frame along one of its own axes by a given distance.

The first argument is a function that returns the axis to translate along, given
the current frame. The majority of the time this argument will be either
`Frame2d.xAxis` or `Frame2d.yAxis`. The second argument is the distance to
translate along the given axis.

This function is convenient when constructing frames via a series of
transformations. For example,

    frame =
        Frame2d.atPoint (Point2d.fromCoordinates ( 2, 0 ))
            |> Frame2d.rotateBy (degrees 45)
            |> Frame2d.translateAlongOwn Frame2d.xAxis 2

means "construct a frame at the point (2, 0), rotate it around its own origin
point by 45 degrees, then translate it along its own X axis by 2 units",
resulting in

    Frame2d.originPoint frame
    --> Point2d.fromCoordinates ( 3.4142, 1.4142 )

    Frame2d.xDirection frame
    --> Direction2d.fromAngle (degrees 45)

    Frame2d.yDirection frame
    --> Direction2d.fromAngle (degrees 135)

-}
translateAlongOwn : (Frame2d -> Axis2d) -> Float -> Frame2d -> Frame2d
translateAlongOwn axis distance frame =
    let
        displacement =
            Vector2d.withLength distance (Axis2d.direction (axis frame))
    in
    translateBy displacement frame


{-| Mirror a frame across an axis.

    frame =
        Frame2d.atPoint (Point2d.fromCoordinates ( 2, 3 ))

    mirroredFrame =
        Frame2d.mirrorAcross Axis2d.x frame

    Frame2d.originPoint mirroredFrame
    --> Point2d.fromCoordinates ( 2, -3 )

    Frame2d.xDirection mirroredFrame
    --> Direction2d.x

    Frame2d.yDirection mirroredFrame
    --> Direction2d.negativeY

Note that this will switch the [handedness](https://en.wikipedia.org/wiki/Cartesian_coordinate_system#Orientation_and_handedness)
of the frame.

-}
mirrorAcross : Axis2d -> Frame2d -> Frame2d
mirrorAcross axis =
    let
        mirrorPoint =
            Point2d.mirrorAcross axis

        mirrorDirection =
            Direction2d.mirrorAcross axis
    in
    \frame ->
        unsafe
            { originPoint = mirrorPoint (originPoint frame)
            , xDirection = mirrorDirection (xDirection frame)
            , yDirection = mirrorDirection (yDirection frame)
            }


{-| Take two frames defined in global coordinates, and return the second one
expressed in local coordinates relative to the first.
-}
relativeTo : Frame2d -> Frame2d -> Frame2d
relativeTo otherFrame =
    let
        relativePoint =
            Point2d.relativeTo otherFrame

        relativeDirection =
            Direction2d.relativeTo otherFrame
    in
    \frame ->
        unsafe
            { originPoint = relativePoint (originPoint frame)
            , xDirection = relativeDirection (xDirection frame)
            , yDirection = relativeDirection (yDirection frame)
            }


{-| Take one frame defined in global coordinates and a second frame defined
in local coordinates relative to the first frame, and return the second frame
expressed in global coordinates.
-}
placeIn : Frame2d -> Frame2d -> Frame2d
placeIn otherFrame =
    let
        placePoint =
            Point2d.placeIn otherFrame

        placeDirection =
            Direction2d.placeIn otherFrame
    in
    \frame ->
        unsafe
            { originPoint = placePoint (originPoint frame)
            , xDirection = placeDirection (xDirection frame)
            , yDirection = placeDirection (yDirection frame)
            }
