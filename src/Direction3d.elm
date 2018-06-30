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


module Direction3d
    exposing
        ( Direction3d
        , angleFrom
        , azimuth
        , componentIn
        , components
        , elevation
        , equalWithin
        , from
        , fromAzimuthAndElevation
        , mirrorAcross
        , negativeX
        , negativeY
        , negativeZ
        , on
        , orthogonalize
        , orthonormalize
        , perpendicularBasis
        , perpendicularTo
        , placeIn
        , positiveX
        , positiveY
        , positiveZ
        , projectInto
        , projectOnto
        , relativeTo
        , reverse
        , rotateAround
        , toVector
        , unsafe
        , x
        , xComponent
        , y
        , yComponent
        , z
        , zComponent
        )

{-| <img src="https://ianmackenzie.github.io/elm-geometry/1.0.0/Direction3d/icon.svg" alt="Direction3d" width="160">

A `Direction3d` represents a direction like 'up' or 'north' or 'forwards'. They
are represented using X, Y and Z components, and can be converted to vectors if
necessary, but should be thought of as conceptually different. Directions have
several uses, such as:

  - Constructing a vector from a length and direction
  - Determining the component of a vector in a particular direction (for
    example, finding the component of velocity in the up direction to get
    vertical speed)
  - Determining the angle between two directions
  - Defining the orientation of an axis, plane or reference frame

@docs Direction3d


# Constants

@docs x, y, z, positiveX, negativeX, positiveY, negativeY, positiveZ, negativeZ


# Constructors

@docs from, on, fromAzimuthAndElevation, perpendicularTo, perpendicularBasis, orthonormalize, orthogonalize, unsafe


# Properties

@docs components, xComponent, yComponent, zComponent, azimuth, elevation


# Comparison

@docs equalWithin


# Measurement

@docs componentIn, angleFrom


# Conversion

@docs toVector


# Transformations

@docs reverse, rotateAround, mirrorAcross, projectOnto


# Coordinate conversions

Functions for transforming directions between local and global coordinates in
different coordinate frames. Like other transformations, coordinate
transformations of directions depend only on the orientations of the relevant
frames, not their positions.

For the examples, assume the following definition of a local coordinate frame,
one that is rotated 30 degrees counterclockwise around the Z axis from the
global XYZ frame:

    rotatedFrame =
        Frame3d.xyz
            |> Frame3d.rotateAround Axis3d.z (degrees 30)

@docs relativeTo, placeIn, projectInto

-}

import Bootstrap.SketchPlane3d as SketchPlane3d
import Direction2d exposing (Direction2d)
import Geometry.Types as Types exposing (Axis3d, Frame3d, Plane3d, Point3d, SketchPlane3d)
import Vector2d exposing (Vector2d)
import Vector3d exposing (Vector3d)


toDirection : Vector3d -> Direction3d
toDirection vector =
    unsafe (Vector3d.components vector)


{-| -}
type alias Direction3d =
    Types.Direction3d


{-| Synonym for `Direction3d.positiveX`.
-}
x : Direction3d
x =
    unsafe ( 1, 0, 0 )


{-| Synonym for `Direction3d.positiveY`.
-}
y : Direction3d
y =
    unsafe ( 0, 1, 0 )


{-| Synonym for `Direction3d.positiveZ`.
-}
z : Direction3d
z =
    unsafe ( 0, 0, 1 )


{-| The positive X direction.

    Direction3d.components Direction3d.positiveX
    --> ( 1, 0, 0 )

-}
positiveX : Direction3d
positiveX =
    unsafe ( 1, 0, 0 )


{-| The negative X direction.

    Direction3d.components Direction3d.negativeX
    --> ( -1, 0, 0 )

-}
negativeX : Direction3d
negativeX =
    unsafe ( -1, 0, 0 )


{-| The positive Y direction.

    Direction3d.components Direction3d.positiveY
    --> ( 0, 1, 0 )

-}
positiveY : Direction3d
positiveY =
    unsafe ( 0, 1, 0 )


{-| The negative Y direction.

    Direction3d.components Direction3d.negativeY
    --> ( 0, -1, 0 )

-}
negativeY : Direction3d
negativeY =
    unsafe ( 0, -1, 0 )


{-| The positive Z direction.

    Direction3d.components Direction3d.positiveZ
    --> ( 0, 0, 1 )

-}
positiveZ : Direction3d
positiveZ =
    unsafe ( 0, 0, 1 )


{-| The negative Z direction.

    Direction3d.components Direction3d.negativeZ
    --> ( 0, 0, -1 )

-}
negativeZ : Direction3d
negativeZ =
    unsafe ( 0, 0, -1 )


{-| Construct a direction directly from its X, Y and Z components. Note that
**you must ensure that the sum of the squares of the given components is exactly
one**:

    Direction3d.unsafe ( 1, 0, 0 )
    Direction3d.unsafe ( 0, -1, 0 )
    Direction3d.unsafe ( 0.6, 0, 0.8 )

are all valid but

    Direction3d.unsafe ( 2, 0, 0 )
    Direction3d.unsafe ( 1, 1, 1 )

are not. Instead of using `Direction3d.unsafe`, it may be easier to use
constructors like `Direction3d.on` or `Direction3d.fromAzimuthAndElevation`
(which will always result in a valid direction), or start with existing
directions and transform them as necessary.

-}
unsafe : ( Float, Float, Float ) -> Direction3d
unsafe =
    Types.Direction3d


{-| Construct a 3D direction lying _on_ a sketch plane by providing a 2D
direction specified in XY coordinates _within_ the sketch plane.

    horizontalDirection =
        Direction3d.on SketchPlane3d.xy <|
            Direction2d.fromAngle (degrees 45)

    Direction3d.components horizontalDirection
    --> ( 0.7071, 0.7071, 0 )

    thirtyDegreesFromZ =
        Direction3d.on SketchPlane3d.zx <|
            Direction2d.fromAngle (degrees 30)

    Direction3d.components thirtyDegreesFromZ
    --> ( 0.5, 0, 0.866 )

-}
on : SketchPlane3d -> Direction2d -> Direction3d
on sketchPlane direction2d =
    let
        ( dx, dy ) =
            Direction2d.components direction2d

        ( ux, uy, uz ) =
            components (SketchPlane3d.xDirection sketchPlane)

        ( vx, vy, vz ) =
            components (SketchPlane3d.yDirection sketchPlane)
    in
    unsafe
        ( dx * ux + dy * vx
        , dx * uy + dy * vy
        , dx * uz + dy * vz
        )


{-| Construct a direction using azimuthal and elevation angles relative to the
global XYZ frame. The azimuth defines the direction's polar angle on the global
XY plane (from X towards Y) and the elevation defines its angle out of the XY
plane towards positive Z.

    Direction3d.components
        (Direction3d.fromAzimuthAndElevation
            (degrees 45)
            (degrees 45)
        )
    --> ( 0.5, 0.5, 0.7071 )

-}
fromAzimuthAndElevation : Float -> Float -> Direction3d
fromAzimuthAndElevation azimuth_ elevation_ =
    let
        cosElevation =
            cos elevation_
    in
    unsafe
        ( cosElevation * cos azimuth_
        , cosElevation * sin azimuth_
        , sin elevation_
        )


{-| Attempt to construct the direction from the first given point to the second.
If the two points are coincident, returns `Nothing`.

    point =
        Point3d.fromCoordinates ( 1, 0, 1 )

    Direction3d.from Point3d.origin point
    --> Just
    -->     (Direction3d.fromAzimuthAndElevation
    -->         (degrees 0)
    -->         (degrees 45)
    -->     )

    Direction3d.from point Point3d.origin
    --> Just
    -->     (Direction3d.fromAzimuthAndElevation
    -->         (degrees 180)
    -->         (degrees -45)
    -->     )

    Direction3d.from point point
    --> Nothing

-}
from : Point3d -> Point3d -> Maybe Direction3d
from firstPoint secondPoint =
    Vector3d.direction (Vector3d.from firstPoint secondPoint)


{-| Construct an arbitrary direction perpendicular to the given direction. The
exact resulting direction is not specified, but it is guaranteed to be
perpendicular to the given direction.

    Direction3d.perpendicularTo Direction3d.x
    --> Direction3d.negativeZ

    Direction3d.perpendicularTo Direction3d.y
    --> Direction3d.positiveZ

    direction =
        Direction3d.fromAzimuthAndElevation
            (degrees 0)
            (degrees 60)

    Direction3d.perpendicularTo direction
    --> Direction3d.fromAzimuthAndElevation
    -->     (degrees 0)
    -->     (degrees -30)

-}
perpendicularTo : Direction3d -> Direction3d
perpendicularTo direction =
    let
        perpendicularVector =
            Vector3d.perpendicularTo (toVector direction)

        length =
            Vector3d.length perpendicularVector

        normalizedVector =
            Vector3d.scaleBy (1 / length) perpendicularVector
    in
    toDirection normalizedVector


{-| Construct a pair of directions that are perpendicular to each other and both
perpendicular to the given direction.

The given direction and the two returned directions will form a
[right-handed](https://en.wikipedia.org/wiki/Cartesian_coordinate_system#Orientation_and_handedness)
system (that is, a right-handed `Frame3d` could be constructed by using the
given direction as the X direction and the two returned directions as the Y and
Z directions, or the given direction as the Z direction and the two returned
directions as the X and Y directions).

    Direction3d.perpendicularBasis Direction3d.x
    --> ( Direction3d.negativeZ
    --> , Direction3d.positiveY
    --> )

    Direction3d.perpendicularBasis Direction3d.y
    --> ( Direction3d.positiveZ
    --> , Direction3d.positiveX
    --> )

-}
perpendicularBasis : Direction3d -> ( Direction3d, Direction3d )
perpendicularBasis direction =
    let
        xDirection =
            perpendicularTo direction

        yDirection =
            Vector3d.crossProduct (toVector direction) (toVector xDirection)
                |> toDirection
    in
    ( xDirection, yDirection )


{-| Attempt to form a set of three mutually perpendicular directions from the
three given vectors by performing [Gram-Schmidt normalization](https://en.wikipedia.org/wiki/Gram%E2%80%93Schmidt_process):

  - The first returned direction will be equal to the direction of the first
    given vector
  - The second returned direction will be as close as possible to the second
    given vector while being perpendicular to the first returned direction
  - The third returned direction will be as close as possible to the third given
    vector while being perpendicular to the first and second returned directions

If any of the given vectors are zero, any two of them are parallel, or the three
are coplanar, returns `Nothing`.

    Direction3d.orthonormalize
        (Vector3d.fromComponents ( 3, 3, 0 ))
        (Vector3d.fromComponents ( 0, 2, 0 ))
        (Vector3d.fromComponents ( 1, 2, 3 ))
    --> Just
    -->     ( Direction3d.fromAzimuthAndElevation
    -->         (degrees 45)
    -->         (degrees 0)
    -->     , Direction3d.fromAzimuthAndElevation
    -->         (degrees 135)
    -->         (degrees 0)
    -->     , Direction3d.positiveZ
    -->     )

    -- Three vectors in the XY plane:
    Direction3d.orthonormalize
        (Vector3d.fromComponents ( 2, 0, 0 ))
        (Vector3d.fromComponents ( 3, 1, 0 ))
        (Vector3d.fromComponents ( 4, 2, 0 ))
    --> Nothing

-}
orthonormalize : Vector3d -> Vector3d -> Vector3d -> Maybe ( Direction3d, Direction3d, Direction3d )
orthonormalize xVector xyVector xyzVector =
    Vector3d.direction xVector
        |> Maybe.andThen
            (\xDirection ->
                let
                    yVector =
                        Vector3d.crossProduct
                            (Vector3d.crossProduct xVector xyVector)
                            xVector
                in
                Vector3d.direction yVector
                    |> Maybe.andThen
                        (\yDirection ->
                            let
                                rightHandedZVector =
                                    Vector3d.crossProduct xVector yVector

                                dotProduct =
                                    Vector3d.dotProduct
                                        xyzVector
                                        rightHandedZVector

                                zVector =
                                    if dotProduct > 0 then
                                        rightHandedZVector
                                    else if dotProduct < 0 then
                                        Vector3d.reverse rightHandedZVector
                                    else
                                        Vector3d.zero
                            in
                            Vector3d.direction zVector
                                |> Maybe.map
                                    (\zDirection ->
                                        ( xDirection
                                        , yDirection
                                        , zDirection
                                        )
                                    )
                        )
            )


{-| Attempt to form a set of three mutually perpendicular directions from the
three given directions;

    Direction3d.orthogonalize
        xDirection
        yDirection
        zDirection

is equivalent to

    Direction3d.orthonormalize
        (Direction3d.toVector xDirection)
        (Direction3d.toVector yDirection)
        (Direction3d.toVector zDirection)

-}
orthogonalize : Direction3d -> Direction3d -> Direction3d -> Maybe ( Direction3d, Direction3d, Direction3d )
orthogonalize xDirection yDirection zDirection =
    orthonormalize
        (toVector xDirection)
        (toVector yDirection)
        (toVector zDirection)


{-| Get the components of a direction as a tuple (the components it would have
as a unit vector, also know as its direction cosines).

    ( x, y, z ) =
        Direction3d.components direction

-}
components : Direction3d -> ( Float, Float, Float )
components (Types.Direction3d components_) =
    components_


{-| Get the X component of a direction.

    Direction3d.xComponent Direction3d.x
    --> 1

    Direction3d.xComponent Direction3d.y
    --> 0

-}
xComponent : Direction3d -> Float
xComponent (Types.Direction3d ( xComponent_, _, _ )) =
    xComponent_


{-| Get the Y component of a direction.

    Direction3d.yComponent Direction3d.y
    --> 1

    Direction3d.yComponent Direction3d.z
    --> 0

-}
yComponent : Direction3d -> Float
yComponent (Types.Direction3d ( _, yComponent_, _ )) =
    yComponent_


{-| Get the Z component of a direction.

    Direction3d.zComponent Direction3d.z
    --> 1

    Direction3d.zComponent Direction3d.x
    --> 0

-}
zComponent : Direction3d -> Float
zComponent (Types.Direction3d ( _, _, zComponent_ )) =
    zComponent_


{-| Find the component of one direction in another direction. This is equal to
the cosine of the angle between the directions, or equivalently the dot product
of the two directions converted to unit vectors.

    direction =
        Direction3d.fromAzimuthAndElevation
            (degrees 0)
            (degrees 60)

    Direction3d.componentIn Direction3d.x direction
    --> 0.5

    Direction3d.componentIn Direction3d.z direction
    --> 0.866

    Direction3d.componentIn direction direction
    --> 1

    direction
        |> Direction3d.componentIn
            (Direction3d.reverse direction)
    --> -1

This is more general and flexible than using `xComponent`, `yComponent` or
`zComponent`, all of which can be expressed in terms of `componentIn`; for
example,

    Direction3d.zComponent direction

is equivalent to

    Direction3d.componentIn Direction3d.z direction

-}
componentIn : Direction3d -> Direction3d -> Float
componentIn firstDirection secondDirection =
    Vector3d.componentIn firstDirection (toVector secondDirection)


{-| Get the angle of a direction in the XY plane, measured from the X axis
towards the Y axis (counterclockwise around the Z axis). The result will be in
the range -π to π.

    Direction3d.azimuth Direction3d.x
    --> 0

    Direction3d.azimuth Direction3d.y
    --> degrees 90

    Direction3d.azimuth Direction3d.negativeY
    --> degrees -90

    Direction3d.azimuth Direction3d.negativeX
    --> degrees 180

Vertical directions are considered to have an azimuth of zero:

    Direction3d.azimuth Direction3d.z
    --> 0

-}
azimuth : Direction3d -> Float
azimuth direction =
    atan2 (yComponent direction) (xComponent direction)


{-| Get the angle of a direction from the XY plane towards positive Z. The
result will be in the range -π/2 to π/2.

    Direction3d.elevation Direction3d.x
    --> 0

    Direction3d.elevation Direction3d.negativeY
    --> 0

    Direction3d.elevation Direction3d.z
    --> degrees 90

    Direction3d.elevation Direction3d.negativeZ
    --> degrees -90

-}
elevation : Direction3d -> Float
elevation direction =
    asin (zComponent direction)


{-| Compare two directions within an angular tolerance. Returns true if the
angle between the two given directions is less than the given tolerance.

    rotatedDirection =
        Direction3d.x
            |> Direction3d.rotateAround Axis3d.z
                (degrees 2)

    Direction3d.equalWithin (degrees 5)
        Direction3d.x
        rotatedDirection
    --> True

    Direction3d.equalWithin (degrees 1)
        Direction3d.x
        rotatedDirection
    --> False

-}
equalWithin : Float -> Direction3d -> Direction3d -> Bool
equalWithin angle firstDirection secondDirection =
    angleFrom firstDirection secondDirection <= angle


{-| Convert a direction to a unit vector.

    Direction3d.toVector Direction3d.y
    --> Vector3d.fromComponents ( 0, 1, 0 )

-}
toVector : Direction3d -> Vector3d
toVector direction =
    Vector3d.fromComponents (components direction)


{-| Find the angle from one direction to another. The result will be in the
range 0 to π.

    Direction3d.angleFrom Direction3d.x Direction3d.x
    --> degrees 0

    Direction3d.angleFrom Direction3d.x Direction3d.z
    --> degrees 90

    Direction3d.angleFrom
        Direction3d.y
        Direction3d.negativeY
    --> degrees 180

-}
angleFrom : Direction3d -> Direction3d -> Float
angleFrom firstDirection secondDirection =
    let
        ( x1, y1, z1 ) =
            components firstDirection

        ( x2, y2, z2 ) =
            components secondDirection

        relativeX =
            x1 * x2 + y1 * y2 + z1 * z2

        cx =
            y1 * z2 - z1 * y2

        cy =
            z1 * x2 - x1 * z2

        cz =
            x1 * y2 - y1 * x2

        relativeY =
            sqrt (cx * cx + cy * cy + cz * cz)
    in
    atan2 relativeY relativeX


{-| Reverse a direction.

    Direction3d.reverse Direction3d.y
    --> Direction3d.negativeY

-}
reverse : Direction3d -> Direction3d
reverse direction =
    let
        ( dx, dy, dz ) =
            components direction
    in
    unsafe ( -dx, -dy, -dz )


{-| Rotate a direction around an axis by a given angle.

    Direction3d.y
        |> Direction3d.rotateAround Axis3d.x (degrees 90)
    --> Direction3d.z

Note that only the direction of the axis affects the result, not the position of
its origin point, since directions are position-independent:

    offsetAxis =
        Axis3d.withDirection Direction3d.z
            (Point3d.fromCoordinates ( 100, 200, 300 ))

    Direction3d.x
        |> Direction3d.rotateAround offsetAxis (degrees 90)
    --> Direction3d.y

-}
rotateAround : Axis3d -> Float -> Direction3d -> Direction3d
rotateAround axis angle direction =
    toVector direction |> Vector3d.rotateAround axis angle |> toDirection


{-| Mirror a direction across a plane.

    direction =
        Direction3d.fromAzimuthAndElevation
            (degrees 30)
            (degrees 60)

    Direction3d.mirrorAcross Plane3d.xy direction
    --> Direction3d.fromAzimuthAndElevation
    -->     (degrees 30)
    -->     (degrees -60)

Note that only the normal direction of the plane affects the result, not the
position of its origin point, since directions are position-independent:

    Direction3d.mirrorAcross Plane3d.yz direction
    --> Direction3d.fromAzimuthAndElevation
    -->     (degrees 150)
    -->     (degrees 60)

    offsetPlane =
        Plane3d.offsetBy 10 Plane3d.yz

    Direction3d.mirrorAcross offsetPlane direction
    --> Direction3d.fromAzimuthAndElevation
    -->     (degrees 150)
    -->     (degrees 60)

-}
mirrorAcross : Plane3d -> Direction3d -> Direction3d
mirrorAcross plane direction =
    toVector direction |> Vector3d.mirrorAcross plane |> toDirection


{-| Find the [orthographic projection](https://en.wikipedia.org/wiki/Orthographic_projection)
of a direction onto a plane (renormalized to have unit length). If the given
direction is exactly perpendicular to the given plane, returns `Nothing`.

    direction =
        Direction3d.fromAzimuthAndElevation
            (degrees -60)
            (degrees 0)

    Direction3d.projectOnto Plane3d.xy direction
    --> Just direction

    Direction3d.projectOnto Plane3d.xz direction
    --> Just Direction3d.x

    Direction3d.projectOnto Plane3d.yz direction
    --> Just Direction3d.negativeY

    Direction3d.projectOnto Plane3d.xy Direction3d.z
    --> Nothing

-}
projectOnto : Plane3d -> Direction3d -> Maybe Direction3d
projectOnto plane direction =
    toVector direction |> Vector3d.projectOnto plane |> Vector3d.direction


{-| Take a direction defined in global coordinates, and return it expressed in
local coordinates relative to a given reference frame.

    Direction3d.relativeTo rotatedFrame Direction3d.x
    --> Direction3d.fromAzimuthAndElevation
    -->     (degrees -30)
    -->     (degrees 0)

    Direction3d.relativeTo rotatedFrame Direction3d.y
    --> Direction3d.fromAzimuthAndElevation
    -->     (degrees 60)
    -->     (degrees 0)

    Direction3d.relativeTo rotatedFrame Direction3d.z
    --> Direction3d.z

-}
relativeTo : Frame3d -> Direction3d -> Direction3d
relativeTo frame direction =
    toVector direction |> Vector3d.relativeTo frame |> toDirection


{-| Take a direction defined in local coordinates relative to a given reference
frame, and return that direction expressed in global coordinates.

    Direction3d.placeIn rotatedFrame Direction3d.x
    --> Direction3d.fromAzimuthAndElevation
    -->     (degrees 30)
    -->     (degrees 0)

    Direction3d.placeIn rotatedFrame Direction3d.y
    --> Direction3d.fromAzimuthAndElevation
    -->     (degrees 120)
    -->     (degrees 0)

    Direction3d.placeIn rotatedFrame Direction3d.z
    --> Direction3d.z

-}
placeIn : Frame3d -> Direction3d -> Direction3d
placeIn frame direction =
    toVector direction |> Vector3d.placeIn frame |> toDirection


{-| Project a direction into a given sketch plane. Conceptually, this finds the
[orthographic projection](https://en.wikipedia.org/wiki/Orthographic_projection)
of the direction onto the plane, re-normalizes it to have unit length, and then
expresses the projected direction in 2D sketch coordinates.

This is only possible if the direction is not perpendicular to the sketch
plane; if it is perpendicular, `Nothing` is returned.

    direction =
        Direction3d.fromAzimuthAndElevation
            (degrees -60)
            (degrees 0)

    Direction3d.projectInto SketchPlane3d.xy direction
    --> Just (Direction2d.fromAngle (degrees -60))

    Direction3d.projectInto SketchPlane3d.xz direction
    --> Just Direction2d.x

    Direction3d.projectInto SketchPlane3d.yz direction
    --> Just Direction2d.negativeX

    Direction3d.projectInto SketchPlane3d.xy Direction3d.z
    --> Nothing

-}
projectInto : SketchPlane3d -> Direction3d -> Maybe Direction2d
projectInto sketchPlane direction =
    toVector direction |> Vector3d.projectInto sketchPlane |> Vector2d.direction
